package net.lag.configgy

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.TokenParsers


class ConfigParser extends TokenParsers {
    type Tokens = ConfigLexer
    val lexical = new Tokens
    
    import lexical.{Delimiter, Number, QuotedString}
    
    def value = number | string //| stringList
    def number = accept("number", { case lexical.Number(x) => if (x.contains('.')) x else Integer.parseInt(x) })
    def string = accept("string", { case lexical.QuotedString(x) => x })
    //def stringList = accept(Delimiter("[")) ~ repsep(string, ",") ~ accept(Delimiter("]")) ^^ { list => list.toArray }
    
    
    def parseValue(in: String) = {
        phrase(value)(new lexical.Scanner(in)) match {
            case Success(result, _) => Some(result)
            case _ => None
        }
    }
}
