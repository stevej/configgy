package net.lag.configgy

import scala.collection.mutable.Stack
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.TokenParsers


/**
 * An exception thrown when parsing a config file, if there was an error
 * during parsing. The <code>reason</code> string will contain the parsing
 * error details.
 */
class ParseException(reason: String) extends Exception(reason)


private[configgy] class ConfigParser(var attr: Attributes, val importer: Importer) extends TokenParsers {
    type Tokens = ConfigLexer
    val lexical = new Tokens
    
    import lexical.{Assign, CloseTag, Delim, Ident, Keyword, Number, OpenTag, QuotedString, TagAttribute}
    
    val sections = new Stack[String]
    var prefix = ""
    
    
    def root = rep(includeFile | assignment | toggle | sectionOpen | sectionClose)
    
    def value = number | string | stringList | onoff | trueFalse
    def number = accept("number", { case Number(x) => if (x.contains('.')) x else Integer.parseInt(x) })
    def string = accept("string", { case QuotedString(x) => attr.interpolate(prefix, x) })
    def stringList = accept(Delim("[")) ~> repsep(string, Delim(",")) <~ accept(Delim("]")) ^^ { list => list.toArray }
    
    def assignment = assignName ~ accept("operation", { case Assign(x) => x }) ~ value ^^ {
        case k ~ a ~ v => if (a match {
            case "=" => true
            case "?=" => ! attr.contains(prefix + k)
        }) v match {
            case x: Int => attr(prefix + k) = x
            case x: String => attr(prefix + k) = x
            case x: Array[String] => attr(prefix + k) = x
            case x: Boolean => attr(prefix + k) = x
        }
    }
    def toggle = assignName ~ (onoff | trueFalse) ^^ { case k ~ v => attr(k) = v } 
    def assignName = accept("key", { case Ident(x) => x })
    def onoff = accept("on/off", {
        case Ident("on") => true
        case Ident("off") => false
    })
    def trueFalse = accept("true/false", {
        case Ident("true") => true
        case Ident("false") => false
    })

    def sectionOpen = accept("open tag", { case x @ OpenTag(name, attrList) => x }) ^^ {
        case OpenTag(name, attrList) => {
            sections += name
            prefix = sections.mkString("", ".", ".")
            val newBlock = attr.makeAttributes(sections.mkString("."))
            for (a <- attrList) a match {
                case TagAttribute("inherit", blockName) => newBlock inheritFrom attr.makeAttributes(blockName)
                case _ => throw new ParseException("Unknown block modifier")
            }
        }
    }
    
    def sectionClose = accept("close tag", { case CloseTag(x) => x }) ^^ {
        case x: String => {
            if (sections.isEmpty) {
                failure("dangling close tag: " + x)
            } else {
                val last = sections.pop
                if (last != x) {
                    failure("got closing tag for " + x + ", expected " + last)
                } else {
                    prefix = sections.mkString(".")
                }
            }
        }
    }
    
    def includeFile = accept(Keyword("include")) ~> string ^^ {
        case filename: String => {
            new ConfigParser(attr.makeAttributes(sections.mkString(".")), importer) parse importer.importFile(filename)
        }
    }
    
    def parse(in: String): Unit = {
        phrase(root)(new lexical.Scanner(in)) match {
            case Success(result, _) => result
            case x @ Failure(msg, _) => throw new ParseException(x.toString)
            case x @ Error(msg, _) => throw new ParseException(x.toString)
        }
    }
}
