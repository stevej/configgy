package net.lag.configgy

import sorg.testing._


object StringUtilsTests extends Tests {
    override def testName = "StringUtilsTests"

    test("quoteC") {
        expect("nothing") { StringUtils.quoteC("nothing") }
        expect("name\\tvalue\\t\\u20acb\\xfcllet?\\u20ac") { StringUtils.quoteC("name\tvalue\t\u20acb\u00fcllet?\u20ac") }
        expect("she said \\\"hello\\\"") { StringUtils.quoteC("she said \"hello\"") }
        expect("\\\\backslash") { StringUtils.quoteC("\\backslash") }
    }
    
    test("unquoteC") {
        expect("nothing") { StringUtils.unquoteC("nothing") }
        expect("name\tvalue\t\u20acb\u00fcllet?\u20ac") { StringUtils.unquoteC("name\\tvalue\\t\\u20acb\\xfcllet?\\u20ac") }
        expect("she said \"hello\"") { StringUtils.unquoteC("she said \\\"hello\\\"") }
        expect("\\backslash") { StringUtils.unquoteC("\\\\backslash") }
        expect("real\\$dollar") { StringUtils.unquoteC("real\\$dollar") }
    }
}
