package net.lag

import sorg.testing._
import net.lag.extensions._


object ConfiggyExtensionsTests extends Tests {
    override def testName = "ConfiggyExtensionsTests"

    test("quoteC") {
        expect("nothing") { "nothing".quoteC }
        expect("name\\tvalue\\t\\u20acb\\xfcllet?\\u20ac") { "name\tvalue\t\u20acb\u00fcllet?\u20ac".quoteC }
        expect("she said \\\"hello\\\"") { "she said \"hello\"".quoteC }
        expect("\\\\backslash") { "\\backslash".quoteC }
    }

    test("unquoteC") {
        expect("nothing") { "nothing".unquoteC }
        expect("name\tvalue\t\u20acb\u00fcllet?\u20ac") { "name\\tvalue\\t\\u20acb\\xfcllet?\\u20ac".unquoteC }
        expect("she said \"hello\"") { "she said \\\"hello\\\"".unquoteC }
        expect("\\backslash") { "\\\\backslash".unquoteC }
        expect("real\\$dollar") { "real\\$dollar".unquoteC }
    }

    test("hexlify") {
        expect("656c6c") { "hello".getBytes.slice(1, 4).force.hexlify }
        expect("68656c6c6f") { "hello".getBytes.hexlify }
    }
}
