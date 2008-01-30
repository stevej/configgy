package net.lag.configgy;

import sorg.testing._
import util.parsing.input.CharArrayReader

        
object ConfigParserTests extends Tests {
    override def testName = "ConfigParserTests"

    def parse(in: String) = {
        val attr = new Attributes(null, "")
        new ConfigParser(attr).parse(in)
        attr
    }

    test("value") {
        expect("{: weight=\"48\" }") {
            parse("weight = 48").toString
        }
    }
    
    test("conditional") {
        expect("{: weight=\"48\" }") {
            parse("weight = 48\n weight ?= 16").toString
        }
    }
    
    test("bool")  {
        expect("{: whiskey=\"true\" wine=\"false\" }") {
            parse("wine off\nwhiskey on\n").toString
        }
    }
    
    test("nested") {
        expect("{: alpha=\"hello\" beta={beta: gamma=\"23\" } }") {
            parse("alpha=\"hello\"\n<beta>\n    gamma=23\n</beta>").toString
        }
    }
}
