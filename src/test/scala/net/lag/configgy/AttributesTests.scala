package net.lag.configgy

import scala.util.Sorting
import org.specs._


object AttributesSpec extends Specification {

    "Attributes" should {
        "set values" in {
            val s = new Attributes(null, "root")
            s.toString mustEqual "{root: }"
            s.set("name", "Communist")
            s.toString mustEqual "{root: name=\"Communist\" }"
            s.set("age", 8)
            s.toString mustEqual "{root: age=\"8\" name=\"Communist\" }"
            s.set("age", 19)
            s.toString mustEqual "{root: age=\"19\" name=\"Communist\" }"
        }

        "get values" in {
            val s = new Attributes(null, "root")
            s("name") = "Communist"
            s("age") = 8
            s.get("name", "") mustEqual "Communist"
            s.getInt("age", 999) mustEqual 8
            s.getInt("unknown", 500) mustEqual 500
            (s("name") match {
                case Some(x) => x
                case None => null
            }) mustEqual "Communist"
            s("age", null) mustEqual "8"
            s("age", "500") mustEqual "8"
            s("unknown", "500") mustEqual "500"
        }

        "set comound values" in {
            val s = new Attributes(null, "")
            s("name") = "Communist"
            s("age") = 8
            s("disposition") = "fighter"
            s("diet.food") = "Meow Mix"
            s("diet.liquid") = "water"
            s("data") = "\r\r\u00ff\u00ff"
            s.toString mustEqual ("{: age=\"8\" data=\"\\r\\r\\xff\\xff\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } " +
                   "disposition=\"fighter\" name=\"Communist\" }")
        }

        "know what it contains" in {
            val s = new Attributes(null, "")
            s("name") = "Communist"
            s("age") = 8
            s("diet.food") = "Meow Mix"
            s("diet.liquid") = "water"
            s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
            s.contains("age") mustBe true
            s.contains("unknown") mustBe false
            s.contains("diet.food") mustBe true
            s.contains("diet.gas") mustBe false
            s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
        }

        "auto-vivify" in {
            val s = new Attributes(null, "")
            s("a.b.c") = 8
            s.toString mustEqual "{: a={a: b={a.b: c=\"8\" } } }"
            s.get("a.d.x") mustBe None
            // shouldn't have changed the attr map:
            s.toString mustEqual "{: a={a: b={a.b: c=\"8\" } } }"
        }

        "compare with ==" in {
            val s = new Attributes(null, "root")
            s("name") = "Communist"
            s("age") = 8
            s("diet.food.dry") = "Meow Mix"
            val t = new Attributes(null, "root")
            t("name") = "Communist"
            t("age") = 8
            t("diet.food.dry") = "Meow Mix"
            s mustEqual t
        }

        "remove values" in {
            val s = new Attributes(null, "")
            s("name") = "Communist"
            s("age") = 8
            s("diet.food") = "Meow Mix"
            s("diet.liquid") = "water"
            s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
            s.remove("diet.food") mustBe true
            s.remove("diet.food") mustBe false
            s.toString mustEqual "{: age=\"8\" diet={diet: liquid=\"water\" } name=\"Communist\" }"
        }

        "convert to a map" in {
            val s = new Attributes(null, "")
            s("name") = "Communist"
            s("age") = 8
            s("disposition") = "fighter"
            s("diet.food") = "Meow Mix"
            s("diet.liquid") = "water"
            val map = s.asMap

            // turn it into a sorted list, so we get a deterministic answer
            val keyList = map.keys.toList.toArray
            Sorting.quickSort(keyList)
            (for (val k <- keyList) yield (k + "=" + map(k))).mkString("{ ", ", ", " }") mustEqual
                "{ age=8, diet.food=Meow Mix, diet.liquid=water, disposition=fighter, name=Communist }"
        }

        "copy" in {
            val s = new Attributes(null, "")
            s("name") = "Communist"
            s("age") = 8
            s("diet.food") = "Meow Mix"
            s("diet.liquid") = "water"
            val t = s.copy

            s.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
            t.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"

            s("diet.food") = "fish"

            s.toString mustEqual "{: age=\"8\" diet={diet: food=\"fish\" liquid=\"water\" } name=\"Communist\" }"
            t.toString mustEqual "{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }"
        }
    }
}
