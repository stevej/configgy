package net.lag.configgy;

import sorg.testing._
import util.parsing.input.CharArrayReader

        
object ConfigParserTests extends Tests {
    override def testName = "ConfigParserTests"

    def parse(in: String) = {
        val attr = new Config
        attr.load(in)
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
    
    test("interpolate") {
        expect("{: horse=\"ed\" word=\"schedule\" }") {
            parse("horse=\"ed\" word=\"sch$(horse)ule\"").toString
        }
        expect("{: firstname=\"Bob\" fullname=\"Bob Columbo\" lastname=\"Columbo\" }") {
            parse("lastname=\"Columbo\" firstname=\"Bob\" fullname=\"$(firstname) $(lastname)\"").toString
        }
    }
    
    test("do not interpolate") {
        expect("{: horse=\"ed\" word=\"sch$(horse)ule\" }") {
            parse("horse=\"ed\" word=\"sch\\$(horse)ule\"").toString
        }
    }
    
    test("nested interpolate") {
        expect("{: alpha={alpha: beta={alpha.beta: greeting=\"frankly yours\" word=\"schedule\" } drink=\"frankly\" horse=\"frank\" } horse=\"ed\" }") {
            parse("horse=\"ed\"\n" +
                  "<alpha>\n" +
                  "    horse=\"frank\"\n" +
                  "    drink=\"$(horse)ly\"\n" +
                  "    <beta>\n" +
                  "        word=\"sch$(horse)ule\"\n" +
                  "        greeting=\"$(alpha.drink) yours\"\n" +
                  "    </beta>\n" +
                  "</alpha>").toString
        }
    }
}
