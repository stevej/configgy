/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

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

    "handle nested blocks in braces" in {
      parse("alpha=\"hello\"\nbeta {\n    gamma=23\n}").toString mustEqual
        "{: alpha=\"hello\" beta={beta: gamma=\"23\" } }"
      parse("alpha=\"hello\"\nbeta {\n    gamma=23\n    toaster on\n}").toString mustEqual
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
      a.getList("home.states").toList.mkString(",") mustEqual "California,Tennessee,Idaho"
      a.getList("home.states")(0) mustEqual "California"
      a.getList("home.regions")(1) mustEqual "southeast"
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
      parse(data) must throwA(new ConfigException(""))
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
      a.getString("upp.ulimit_fd", "9") mustEqual "32768"
      a.getString("upp.uid", "100") mustEqual "23"
    }

    "inherit using braces" in {
      val data =
        "daemon {\n" +
        "    ulimit_fd = 32768\n" +
        "    uid = 16\n" +
        "}\n" +
        "\n" +
        "upp (inherit=\"daemon\") {\n" +
        "    uid = 23\n" +
        "}\n"
      val a = parse(data)
      a.toString mustEqual "{: daemon={daemon: uid=\"16\" ulimit_fd=\"32768\" } upp={upp (inherit=daemon): uid=\"23\" } }"
      a.getString("upp.ulimit_fd", "9") mustEqual "32768"
      a.getString("upp.uid", "100") mustEqual "23"
    }

    "use parent scope for lookups" in {
      val data =
        "<daemon><inner>\n" +
        "  <common>\n" +
        "    ulimit_fd = 32768\n" +
        "    uid = 16\n" +
        "  </common>\n" +
        "  <upp inherit=\"common\">\n" +
        "    uid = 23\n" +
        "  </upp>\n" +
        "  <slac inherit=\"daemon.inner.common\">\n" +
        "  </slac>\n" +
        "</inner></daemon>\n"
      val a = parse(data)
      a.toString mustEqual "{: daemon={daemon: inner={daemon.inner: common={daemon.inner.common: uid=\"16\" ulimit_fd=\"32768\" } " +
        "slac={daemon.inner.slac (inherit=daemon.inner.common): } upp={daemon.inner.upp (inherit=daemon.inner.common): uid=\"23\" } } } }"
      a.getString("daemon.inner.upp.ulimit_fd", "9") mustEqual "32768"
      a.getString("daemon.inner.upp.uid", "100") mustEqual "23"
      a.getString("daemon.inner.slac.uid", "100") mustEqual "16"
    }

    "handle camel case id in block" in {
      val data =
        "<daemon>\n" +
        "    useLess = 3\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: useless=\"3\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useLess", "14") mustEqual "3"
    }

    "handle dash block" in {
      val data =
        "<daemon>\n" +
        "    <base-dat>\n" +
        "        ulimit_fd = 32768\n" +
        "    </base-dat>\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: base-dat={daemon.base-dat: ulimit_fd=\"32768\" } } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.base-dat.ulimit_fd", "14") mustEqual "32768"
    }

    "handle camelcase block" in {
      val data =
        "<daemon>\n" +
        "    <baseDat>\n" +
        "        ulimit_fd = 32768\n" +
        "    </baseDat>\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: basedat={daemon.baseDat: ulimit_fd=\"32768\" } } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.baseDat.ulimit_fd", "14") mustEqual "32768"
    }

    "handle assignment after block" in {
      val data =
        "<daemon>\n" +
        "    <base>\n" +
        "        ulimit_fd = 32768\n" +
        "    </base>\n" +
        "    useless = 3\n" +
        "</daemon>\n"
      val exp =
        "{: daemon={daemon: base={daemon.base: ulimit_fd=\"32768\" } useless=\"3\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useless", "14") mustEqual "3"
      a.getString("daemon.base.ulimit_fd", "14") mustEqual "32768"
    }

    "two consecutive groups" in {
      val data =
        "<daemon>\n" +
        "    useless = 3\n" +
        "</daemon>\n" +
        "\n" +
        "<upp inherit=\"daemon\">\n" +
        "    uid = 16\n" +
        "</upp>\n"
      val exp =
        "{: daemon={daemon: useless=\"3\" } " +
        "upp={upp (inherit=daemon): uid=\"16\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useless", "14") mustEqual "3"
      a.getString("upp.uid", "1") mustEqual "16"
    }

   "handle a complex case" in {
      val data =
        "<daemon>\n" +
        "    useLess = 3\n" +
        "    <base-dat>\n" +
        "        ulimit_fd = 32768\n" +
        "    </base-dat>\n" +
        "</daemon>\n" +
        "\n" +
        "<upp inherit=\"daemon.base-dat\">\n" +
        "    uid = 16\n" +
        "    <alpha inherit=\"upp\">\n" +
        "        name=\"alpha\"\n" +
        "    </alpha>\n" +
        "    <beta inherit=\"daemon\">\n" +
        "        name=\"beta\"\n" +
        "    </beta>\n" +
        "    someInt=1\n" +
        "</upp>\n"
      val exp =
        "{: daemon={daemon: base-dat={daemon.base-dat: ulimit_fd=\"32768\" } useless=\"3\" } " +
        "upp={upp (inherit=daemon.base-dat): alpha={upp.alpha (inherit=upp): name=\"alpha\" } " +
        "beta={upp.beta (inherit=daemon): name=\"beta\" } someint=\"1\" uid=\"16\" } }"
      val a = parse(data)
      a.toString mustEqual exp
      a.getString("daemon.useLess", "14") mustEqual "3"
      a.getString("upp.uid", "1") mustEqual "16"
      a.getString("upp.ulimit_fd", "1024") mustEqual "32768"
      a.getString("upp.name", "23") mustEqual "23"
      a.getString("upp.alpha.name", "") mustEqual "alpha"
      a.getString("upp.beta.name", "") mustEqual "beta"
      a.getString("upp.alpha.ulimit_fd", "") mustEqual "32768"
      a.getString("upp.beta.useless", "") mustEqual "3"
      a.getString("upp.alpha.useless", "") mustEqual ""
      a.getString("upp.beta.ulimit_fd", "") mustEqual ""
      a.getString("upp.someInt", "4") mustEqual "1"
    }
  }
}
