package net.lag.configgy

import sorg.testing._
import util.parsing.input.CharArrayReader


object ConfigLexerTests extends Tests {

    override def testName = "ConfigLexerTests"

    test("lexer1") {
        expect("OpenTag(hello);EOF") {
            (new ConfigLexer).scan("<hello>").mkString(";")
        }
        
        expect("Number(98.5);EOF") {
            (new ConfigLexer).scan("98.5  ").mkString(";")
        }

        expect("QuotedString(\"hello there\");EOF") {
            (new ConfigLexer).scan("\"hell\\x6F th\\x65re\"").mkString(";")
        }
    }
    
    test("lexer2") {
        expect("Ident(alpha);Assign(=);Number(-4);Delim([);Ident(and);Assign(?=);QuotedString(\"ok\");Assign(=);Keyword(include);EOF") {
            (new ConfigLexer).scan("alpha = -4 [and ?= \"ok\" = include").mkString(";")
        }
    }

}
