package net.lag.configgy

import scala.collection._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.{CharArrayReader, Reader}
import scala.util.parsing.syntax.Tokens
import net.lag.extensions._


/**
 * Tokenize chars (from a string) for use in parsing a config file. In scala,
 * a "Parser" converts things from type A to type B, so in that sense, a
 * lexer is a "Parser" that converts chars to tokens. This is an
 * implementation detail of ConfigParser.
 */
private[configgy] class ConfigLexer extends Lexical with Tokens {

    //  -----  tokens:

    case class Number(chars: String) extends Token {
        override def toString = "Number(" + chars + ")"
    }

    case class Ident(chars: String) extends Token {
        override def toString = "Ident(" + chars + ")"
    }

    case class TagAttribute(name: String, value: String) extends Token {
        override def toString = "TagAttribute(" + name + "," + value + ")"
        override def chars = name + "=" + value
    }

    case class OpenTag(chars: String, attrs: List[TagAttribute]) extends Token {
        override def toString = "OpenTag(" + chars + "," + attrs + ")"
    }

    case class CloseTag(chars: String) extends Token {
        override def toString = "CloseTag(" + chars + ")"
    }

    case class Assign(chars: String) extends Token {
        override def toString = "Assign(" + chars + ")"
    }

    case class QuotedString(chars: String) extends Token {
        override def toString = "QuotedString(\"" + chars.quoteC + "\")"
    }

    case class Delim(chars: String) extends Token {
        override def toString = "Delim(" + chars + ")"
    }

    case class Keyword(chars: String) extends Token {
        override def toString = "Keyword(" + chars + ")"
    }


    // helper function for turning match combos into strings
    def pack(x: Any): String = x match {
        case s: String => s
        case c: Char => c.toString
        case l: List[_] => (for (item <- l) yield pack(item)).mkString("")
        case a ~ b => pack(a) + pack(b)
        case Some(value) => pack(value)
        case None => ""
    }

    // helper function for matching an entire string (not just a char)
    def str(s: String): Parser[String] = accept(s.toList) ^^ { (list) => s }


    val empties = Set[Char]() ++ " \t\n\r".toArray
    override def whitespace = rep((elem("whitespace", empties.contains(_)) +) | ('#' ~ rep(chrExcept('\n')) ~ '\n'))

    override def token: Parser[Token] = number | ident | openTag | closeTag | assign | quotedString | delim | fini

    def any = elem("any", ch => true)
    val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
    def hexDigit = elem("hex digit", hexDigits.contains(_))

    def number = opt(elem('-')) ~ rep1(digit) ~ opt(elem('.') ~ rep(digit)) ^^ { x: Any => new Number(pack(x)) }
    def segment = (letter | elem('_')) ~ rep(letter | digit | elem('-') | elem('_')) ^^ { pack(_) }
    def ident = segment ~ rep(elem('.') ~ segment) ^^ {
        x: Any => pack(x) match {
            case kw @ "include" => new Keyword(kw)
            case ident => new Ident(ident)
        }
    }

    def tagName = letter ~ rep(letter | digit | elem('-') | elem('_')) ^^ { pack(_) }
    def quotedVal = '"' ~ rep(chrExcept('"')) ~ '"' ^^ { case _ ~ x ~ _ => pack(x) }

    def tagAttribute = whitespace ~ tagName ~ '=' ~ quotedVal ^^ {
        case _ ~ name ~ _ ~ value => new TagAttribute(name, value)
    }

    def openTag = '<' ~ tagName ~ rep(tagAttribute) ~ '>' ^^ {
        case _ ~ name ~ attrs ~ _ => new OpenTag(name, attrs)
    }
    def closeTag = '<' ~> '/' ~> tagName <~ '>' ^^ { new CloseTag(_) }

    def assign = (str("=") | str("?=")) ^^ { case x => new Assign(x) }

    def quotedString = '"' ~> rep(chrExcept('\\', '"') | (elem('\\') ~ chrExcept('u', 'x')) |
        (elem('\\') ~ elem('\n')) | (elem('\\') ~ elem('u') ~ repN(4, hexDigit)) |
        (elem('\\') ~ elem('x') ~ repN(2, hexDigit))) <~ '"' ^^ { x: Any => new QuotedString(pack(x).unquoteC) }

    def delim = (str("[") | str("]") | str(",")) ^^ { case x => new Delim(x) }

    def fini = CharArrayReader.EofCh ^^ { case _ => EOF }

    // for unit tests: scan a string and return a list of Tokens
    def scan(s: String): List[Token] = {
        scan(new Scanner(s))
    }
    def scan(scanner: Scanner): List[Token] = {
        if (scanner.atEnd) {
            List[Token](scanner.first)
        } else {
            scanner.first :: scan(scanner.rest)
        }
    }
}
