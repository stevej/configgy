package net.lag.configgy

import org.specs._


object ConfigParserSpec extends Specification {

  class FakeImporter extends Importer {
    def importFile(filename: String): String = {
      filename match {
        case "test1" =>
          "staff = \"weird skull\"\n"
        case "test2" =>
          "<inner>\n" +
          "    cat=\"meow\"\n" +
          "    include \"test3\"\n" +
          "    dog ?= \"blah\"\n" +
          "</inner>"
        case "test3" =>
          "dog=\"bark\"\n" +
          "cat ?= \"blah\"\n"
        case "test4" =>
          "cow=\"moo\"\n"
      }
    }
  }

  def parse(in: String) = {
    val attr = new Config
    attr.importer = new FakeImporter
    attr.load(in)
    attr
  }


  "ConfigParser" should {
    "parse assignment" in {
      parse("weight = 48").toString mustEqual "{: weight=\"48\" }"
    }

    "parse conditional assignment" in {
      parse("weight = 48\n weight ?= 16").toString mustEqual "{: weight=\"48\" }"
    }

    "ignore comments" in {
      parse("# doing stuff\n  weight = 48\n  # more comments\n").toString mustEqual "{: weight=\"48\" }"
    }

    "parse booleans" in {
      parse("wine off\nwhiskey on\n").toString mustEqual "{: whiskey=\"true\" wine=\"false\" }"
      parse("wine = false\nwhiskey = on\n").toString mustEqual "{: whiskey=\"true\" wine=\"false\" }"
    }

    "handle nested blocks" in {
      parse("alpha=\"hello\"\n<beta>\n    gamma=23\n</beta>").toString mustEqual
        "{: alpha=\"hello\" beta={beta: gamma=\"23\" } }"
      parse("alpha=\"hello\"\n<beta>\n    gamma=23\n    toaster on\n</beta>").toString mustEqual
        "{: alpha=\"hello\" beta={beta: gamma=\"23\" toaster=\"true\" } }"
    }

    "handle string lists" in {
      val data =
        "<home>\n" +
        "    states = [\"California\", \"Tennessee\", \"Idaho\"]\n" +
        "    regions = [\"pacific\", \"southeast\", \"northwest\"]\n" +
        "</home>\n"
      val a = parse(data)
      a.toString mustEqual "{: home={home: regions=[pacific,southeast,northwest] states=[California,Tennessee,Idaho] } }"
      a.getStringList("home.states").get.toList.mkString(",") mustEqual "California,Tennessee,Idaho"
      a.getStringList("home.states").getOrElse(null)(0) mustEqual "California"
      a.getStringList("home.regions").getOrElse(null)(1) mustEqual "southeast"
    }

    "import files" in {
      val data1 =
        "toplevel=\"skeletor\"\n" +
        "<inner>\n" +
        "    include \"test1\"\n" +
        "    home = \"greyskull\"\n" +
        "</inner>\n"
      parse(data1).toString mustEqual "{: inner={inner: home=\"greyskull\" staff=\"weird skull\" } toplevel=\"skeletor\" }"

      val data2 =
        "toplevel=\"hat\"\n" +
        "include \"test2\"\n" +
        "include \"test4\"\n"
      parse(data2).toString mustEqual "{: cow=\"moo\" inner={inner: cat=\"meow\" dog=\"bark\" } toplevel=\"hat\" }"
    }

    "refuse to overload key types" in {
      val data =
        "cat = 23\n" +
        "<cat>\n" +
        "    dog = 1\n" +
        "</cat>\n"
      parse(data) must throwA(new AttributesException(""))
    }

    "catch unknown block modifiers" in {
      parse("<upp name=\"fred\">\n</upp>\n") must throwA(new ParseException(""))
    }
  }


  "ConfigParser interpolation" should {
    "interpolate strings" in {
      parse("horse=\"ed\" word=\"sch$(horse)ule\"").toString mustEqual
        "{: horse=\"ed\" word=\"schedule\" }"
      parse("lastname=\"Columbo\" firstname=\"Bob\" fullname=\"$(firstname) $(lastname)\"").toString mustEqual
        "{: firstname=\"Bob\" fullname=\"Bob Columbo\" lastname=\"Columbo\" }"
    }

    "not interpolate unassigned strings" in {
      parse("horse=\"ed\" word=\"sch\\$(horse)ule\"").toString mustEqual "{: horse=\"ed\" word=\"sch$(horse)ule\" }"
    }

    "interpolate nested references" in {
      parse("horse=\"ed\"\n" +
            "<alpha>\n" +
            "    horse=\"frank\"\n" +
            "    drink=\"$(horse)ly\"\n" +
            "    <beta>\n" +
            "        word=\"sch$(horse)ule\"\n" +
            "        greeting=\"$(alpha.drink) yours\"\n" +
            "    </beta>\n" +
            "</alpha>").toString mustEqual
              "{: alpha={alpha: beta={alpha.beta: greeting=\"frankly yours\" word=\"schedule\" } drink=\"frankly\" horse=\"frank\" } horse=\"ed\" }"
    }

    "interpolate environment vars" in {
      parse("user=\"$(USER)\"").toString must beDifferent("{: user=\"$(USER)\" }")
    }
  }


  "ConfigParser inheritance" should {
    "inherit" in {
      val data =
        "<daemon>\n" +
        "    ulimit_fd = 32768\n" +
        "    uid = 16\n" +
        "</daemon>\n" +
        "\n" +
        "<upp inherit=\"daemon\">\n" +
        "    uid = 23\n" +
        "</upp>\n"
      val a = parse(data)
      a.toString mustEqual "{: daemon={daemon: uid=\"16\" ulimit_fd=\"32768\" } upp={upp (inherit=daemon): uid=\"23\" } }"
      a.get("upp.ulimit_fd", "9") mustEqual "32768"
      a.get("upp.uid", "100") mustEqual "23"
    }

    "handle a complex case" in {
      val data =
        "<daemon>\n" +
        "    useless = 3\n" +
        "    <base>\n" +
        "        ulimit_fd = 32768\n" +
        "    </base>\n" +
        "</daemon>\n" +
        "\n" +
        "<upp inherit=\"daemon.base\">\n" +
        "    uid = 16\n" +
        "    <alpha inherit=\"upp\">\n" +
        "        name=\"alpha\"\n" +
        "    </alpha>\n" +
        "    <beta inherit=\"daemon\">\n" +
        "        name=\"beta\"\n" +
        "    </beta>\n" +
        "</upp>\n"
      val exp =
        "{: daemon={daemon: base={daemon.base: ulimit_fd=\"32768\" } useless=\"3\" } " +
        "upp={upp (inherit=daemon.base): alpha={upp.alpha (inherit=upp): name=\"alpha\" } " +
        "beta={upp.beta (inherit=daemon): name=\"beta\" } uid=\"16\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.get("daemon.useless", "14") mustEqual "3"
      a.get("upp.uid", "1") mustEqual "16"
      a.get("upp.ulimit_fd", "1024") mustEqual "32768"
      a.get("upp.name", "23") mustEqual "23"
      a.get("upp.alpha.name", "") mustEqual "alpha"
      a.get("upp.beta.name", "") mustEqual "beta"
      a.get("upp.alpha.ulimit_fd", "") mustEqual "32768"
      a.get("upp.beta.useless", "") mustEqual "3"
      a.get("upp.alpha.useless", "") mustEqual ""
      a.get("upp.beta.ulimit_fd", "") mustEqual ""
    }
  }
}
