package net.lag.configgy

import java.util.regex.Pattern

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.input.{CharArrayReader, Reader}
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.syntax.Tokens


class ConfigLexer extends Lexical with Tokens {

    case class Number(chars: String) extends Token {
        override def toString = "Number(" + chars + ")"
    }
    
    case class Ident(chars: String) extends Token {
        override def toString = "Ident(" + chars + ")"
    }
    
    case class OpenTag(chars: String) extends Token {
        override def toString = "OpenTag(" + chars +  ")"
    }
    
    case class CloseTag(chars: String) extends Token {
        override def toString = "CloseTag(" + chars + ")"
    }
    
    case class Assign(chars: String) extends Token {
        override def toString = "Assign(" + chars + ")"
    }
    
    case class QuotedString(chars: String) extends Token {
        override def toString = "QuotedString(\"" + StringUtils.quoteC(chars) + "\")"
    }
    
    case class Delim(chars: String) extends Token {
        override def toString = "Delim(" + chars + ")"
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
    def str(s: String): Parser[String] = accept(s.toList) ^^ { s }

    
    val empties = Set[Char]() ++ " \t\n\r".toArray
    override def whitespace = rep((elem("whitespace", empties.contains(_)) +) |
        ('#' ~ rep(chrExcept('\n')) ~ '\n'))
    
    override def token: Parser[Token] = number | ident | openTag | closeTag | assign | quotedString | delim | fini
    
    def any = elem("any", ch => true)
    val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray
    def hexDigit = elem("hex digit", hexDigits.contains(_))
    
    def number = opt(elem('-')) ~ rep1(digit) ~ opt(elem('.') ~ rep(digit)) ^^ { x: Any => new Number(pack(x)) }
    def segment = (letter | elem('_')) ~ rep(letter | digit | elem('-') | elem('_')) ^^ { pack(_) }
    def ident = segment ~ rep(elem('.') ~ segment) ^^ { x: Any => new Ident(pack(x)) }

    def tagName = letter ~ rep(letter | digit | elem('-') | elem('_')) ^^ { pack(_) }
    def openTag = '<' ~ tagName ~ '>' ^^ { new OpenTag(_) }
    def closeTag = '<' ~ '/' ~ tagName ~ '>' ^^ { new CloseTag(_) }
    
    def assign = (str("=") | str("?=")) ^^ { case x => new Assign(x) }
    
    def quotedString = '"' ~ rep(chrExcept('\\', '"') | (elem('\\') ~ chrExcept('u', 'x')) |
        (elem('\\') ~ elem('\n')) | (elem('\\') ~ elem('u') ~ repN(4, hexDigit)) |
        (elem('\\') ~ elem('x') ~ repN(2, hexDigit))) ~ '"' ^^ { x: Any => new QuotedString(StringUtils.unquoteC(pack(x))) }
    
    def delim = (str("[") | str("]") | str(",")) ^^ { case x => new Delim(x) }
    
    def fini = EofCh ^^ EOF
    
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
