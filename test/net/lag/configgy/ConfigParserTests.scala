package net.lag.configgy;

import sorg.testing._
import util.parsing.input.CharArrayReader

        
object ConfigParserTests extends Tests {
    override def testName = "ConfigParserTests"
        
    private val TEST_DATA1 =
        "toplevel=\"skeletor\"\n" +
        "<inner>\n" +
        "    include \"test1\"\n" +
        "    home = \"greyskull\"\n" +
        "</inner>\n"
    
    private val IMPORT_DATA1 =
        "staff = \"weird skull\"\n"
    
    private val TEST_DATA2 =
        "toplevel=\"hat\"\n" +
        "include \"test2\"\n" +
        "include \"test4\"\n"

    private val IMPORT_DATA2 =
        "<inner>\n" +
        "    cat=\"meow\"\n" +
        "    include \"test3\"\n" +
        "    dog ?= \"blah\"\n" +
        "</inner>"
   
    private val IMPORT_DATA3 =
        "dog=\"bark\"\n" +
        "cat ?= \"blah\"\n"
    
    private val IMPORT_DATA4 =
        "cow=\"moo\"\n"


    class FakeImporter extends Importer {
        def importFile(filename: String): String = {
            filename match {
                case "test1" => IMPORT_DATA1
                case "test2" => IMPORT_DATA2
                case "test3" => IMPORT_DATA3
                case "test4" => IMPORT_DATA4
            }
        }
    }
    
    def parse(in: String) = {
        val attr = new Config
        attr.importer = new FakeImporter
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
    
    test("env") {
        // not really a test, since we can't guarantee anything from the env
        expect(false) {
            parse("user=\"$(USER)\"").toString == "{: user=\"$(USER)\" }"
        }
    }

    test("import") {
        expect("{: inner={inner: home=\"greyskull\" staff=\"weird skull\" } toplevel=\"skeletor\" }") {
            parse(TEST_DATA1).toString
        }

        expect("{: cow=\"moo\" inner={inner: cat=\"meow\" dog=\"bark\" } toplevel=\"hat\" }") {
            parse(TEST_DATA2).toString
        }
    }
}
