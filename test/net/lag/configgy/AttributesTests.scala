package net.lag.configgy

import scala.util.Sorting

import sorg.testing._


object AttributesTests extends Tests {
    
    override def testName = "AttributesTests"

    test("simple") {
        val s = new Attributes(null, "root")
        expect("{root: }") { s.toString }
        s.set("name", "Communist")
        expect("{root: name=\"Communist\" }") { s.toString }
        s.set("age", 8)
        expect("{root: age=\"8\" name=\"Communist\" }") { s.toString }
        s.set("age", 19)
        expect("{root: age=\"19\" name=\"Communist\" }") { s.toString }
    }
    
    test("read") {
        val s = new Attributes(null, "root")
        s("name") = "Communist"
        s("age") = 8
        expect("Communist") { s.get("name", "") }
        expect(8) { s.getInt("age", 999) }
        expect(500) { s.getInt("unknown", 500) }
        expect("Communist") {
            s("name") match {
                case Some(x) => x
                case None => null
            }
        }
        expect("8") { s("age", null) }
        expect("8") { s("age", "500") }
        expect("500") { s("unknown", "500") }
    }
    
    test("compound") {
        val s = new Attributes(null, "")
        s("name") = "Communist"
        s("age") = 8
        s("disposition") = "fighter"
        s("diet.food") = "Meow Mix"
        s("diet.liquid") = "water"
        s("data") = "\r\r\u00ff\u00ff"
        expect("{: age=\"8\" data=\"\\r\\r\\xff\\xff\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } " +
               "disposition=\"fighter\" name=\"Communist\" }") { s.toString }
    }
    
    test("contains") {
        val s = new Attributes(null, "")
        s("name") = "Communist"
        s("age") = 8
        s("diet.food") = "Meow Mix"
        s("diet.liquid") = "water"
        expect("{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }") { s.toString }
        expect(true) { s.contains("age") }
        expect(false) { s.contains("unknown") }
        expect(true) { s.contains("diet.food") }
        expect(false) { s.contains("diet.gas") }
        expect("{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }") { s.toString }
    }
    
    test("auto-vivify") {
        val s = new Attributes(null, "")
        s("a.b.c") = 8
        expect("{: a={a: b={a.b: c=\"8\" } } }") { s.toString }
        expect(None) { s.get("a.d.x") }
        // shouldn't have changed the attr map:
        expect("{: a={a: b={a.b: c=\"8\" } } }") { s.toString }
    }
    
    test("equals") {
        val s = new Attributes(null, "root")
        s("name") = "Communist"
        s("age") = 8
        s("diet.food.dry") = "Meow Mix"
        val t = new Attributes(null, "root")
        t("name") = "Communist"
        t("age") = 8
        t("diet.food.dry") = "Meow Mix"
        
        expect(true) { s == t }
    }
    
    test("remove") {
        val s = new Attributes(null, "")
        s("name") = "Communist"
        s("age") = 8
        s("diet.food") = "Meow Mix"
        s("diet.liquid") = "water"
        expect("{: age=\"8\" diet={diet: food=\"Meow Mix\" liquid=\"water\" } name=\"Communist\" }") { s.toString }
        expect(true) { s.remove("diet.food") }
        expect(false) { s.remove("diet.food") }
        expect("{: age=\"8\" diet={diet: liquid=\"water\" } name=\"Communist\" }") { s.toString }
    }

    test("as map") {
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
        expect("{ age=8, diet.food=Meow Mix, diet.liquid=water, disposition=fighter, name=Communist }") {
            (for (val k <- keyList) yield (k + "=" + map(k))).mkString("{ ", ", ", " }")
        }
    }
}
