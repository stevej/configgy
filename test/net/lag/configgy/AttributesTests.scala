package net.lag.configgy

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
    }
    
    test("read") {
        val s = new Attributes(null, "root")
        s("name") = "Communist"
        s("age") = 8
        expect("Communist") { s.get("name", "") }
        expect(8) { s.getInt("age", 999) }
        expect(500) { s.getInt("unknown", 500) }
        expect("Communist") { s("name") }
        expect("8") { s("age") }
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
    
    test("auto-vivify") {
        val s = new Attributes(null, "")
        s("a.b.c") = 8
        expect("{: a={a: b={a.b: c=\"8\" } } }") { s.toString }
        expect(None) { s.get("a.d.x") }
        // shouldn't have changed the attr map:
        expect("{: a={a: b={a.b: c=\"8\" } } }") { s.toString }
    }
}
