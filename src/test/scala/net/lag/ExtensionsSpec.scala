/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

package net.lag

import net.lag.extensions._
import org.specs._


object ExtensionsSpec extends Specification {
  "extensions" should {
    "quoteC" in {
      "nothing".quoteC mustEqual "nothing"
      "name\tvalue\t\u20acb\u00fcllet?\u20ac".quoteC mustEqual "name\\tvalue\\t\\u20acb\\xfcllet?\\u20ac"
      "she said \"hello\"".quoteC mustEqual "she said \\\"hello\\\""
      "\\backslash".quoteC  mustEqual "\\\\backslash"
    }

    "unquoteC" in {
      "nothing".unquoteC mustEqual "nothing"
      "name\\tvalue\\t\\u20acb\\xfcllet?\\u20ac".unquoteC  mustEqual "name\tvalue\t\u20acb\u00fcllet?\u20ac"
      "she said \\\"hello\\\"".unquoteC mustEqual "she said \"hello\""
      "\\\\backslash".unquoteC mustEqual "\\backslash"
      "real\\$dollar".unquoteC mustEqual "real\\$dollar"
      "silly\\/quote".unquoteC mustEqual "silly/quote"
    }

    "hexlify" in {
      "hello".getBytes.slice(1, 4).force.hexlify mustEqual "656c6c"
      "hello".getBytes.hexlify mustEqual "68656c6c6f"
    }

    "unhexlify" in {
      "656c6c".unhexlify.toList mustEqual "hello".getBytes.slice(1, 4).force.toList
      "68656c6c6f".unhexlify.toList mustEqual "hello".getBytes.toList
    }
  }
}
