package net.lag.logging

import java.util.{logging => javalog}
import sorg.testing._


object Crazy {
    def cycle(n: Int): Unit = {
        if (n == 0) {
            throw new Exception("Aie!")
        } else {
            cycle(n - 1)
            Logger.get("").trace("nothing")
        }
    }
    
    def cycle2(n: Int): Unit = {
        try {
            cycle(n)
        } catch {
            case t: Throwable => throw new Exception("grrrr", t)
        }
    }
}


class TimeWarpingStringHandler extends StringHandler {
    override def publish(record: javalog.LogRecord) = {
        record.setMillis(1206769996722L)
        super.publish(record)
    }
}


object LoggingTests extends Tests {
    
    override def testName = "LoggingTests"
    
    private var handler = new StringHandler

    override def setUp = {
        Logger.clearHandlers
        handler = new TimeWarpingStringHandler
        Logger.get("").addHandler(handler)
    }
    
    override def tearDown = {
        Logger.clearHandlers
    }
    
        
    // turn logged console lines into a list of repeatable strings
    private def eat(in: String) = {
        in.split("\n").toList
    }
        
    test("simple") {
        val log = Logger.get("")
        log.error("error!")
        expect(List("ERR [20080328-22:53:16.722] (root): error!")) { eat(handler.toString) }
    }
    
    // verify that we can ask logs to be written in UTC
    test("utc") {
        val log = Logger.get("")
        log.getHandlers()(0).asInstanceOf[Handler].use_utc = true
        log.error("error!")
        expect(List("ERR [20080329-05:53:16.722] (root): error!")) { eat(handler.toString) }
    }
    
    test("packages") {
        val log1 = Logger.get("net.lag.logging.Skeletor")
        log1.warning("I am coming for you!")
        val log2 = Logger.get("net.lag.configgy.Skeletor")
        log2.warning("I am also coming for you!")
        
        expect(List("WAR [20080328-22:53:16.722] logging: I am coming for you!",
                    "WAR [20080328-22:53:16.722] configgy: I am also coming for you!")) {
            eat(handler.toString)
        }
    }
    
    test("levels") {
        val log1 = Logger.get("net.lag.logging.Skeletor")
        log1.setLevel(DEBUG)
        log1.warning("I am coming for you!")
        log1.debug("Loading supplies...")
        log1.trace("Catfood query.")
        log1.error("Help!")
        
        expect(List("WAR [20080328-22:53:16.722] logging: I am coming for you!",
                    "DEB [20080328-22:53:16.722] logging: Loading supplies...",
                    "ERR [20080328-22:53:16.722] logging: Help!")) {
            eat(handler.toString)
        }
    }
    
    test("truncate") {
        handler.truncate_at = 30
        val log1 = Logger.get("net.lag.whiskey.Train")
        log1.critical("Something terrible happened that may take a very long time to explain because I write crappy log messages.")
        
        expect(List("CRI [20080328-22:53:16.722] whiskey: Something terrible happened th...")) {
            eat(handler.toString)
        }
    }
    
    test("stack traces") {
        handler.truncate_stack_traces_at = 5
        val log1 = Logger.get("net.lag.whiskey.Train")
        try {
            Crazy.cycle(10)
        } catch {
            case t: Throwable => log1.error(t, "Exception!")
        }
        
        expect(List("ERR [20080328-22:53:16.722] whiskey: Exception!",
                    "ERR [20080328-22:53:16.722] whiskey: java.lang.Exception: Aie!",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:10)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:12)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:12)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:12)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:12)",
                    "ERR [20080328-22:53:16.722] whiskey:     (...more...)")) {
            eat(handler.toString)
        }
    }
    
    test("stack traces 2") {
        handler.truncate_stack_traces_at = 2
        val log1 = Logger.get("net.lag.whiskey.Train")
        try {
            Crazy.cycle2(2)
        } catch {
            case t: Throwable => log1.error(t, "Exception!")
        }
        
        expect(List("ERR [20080328-22:53:16.722] whiskey: Exception!",
                    "ERR [20080328-22:53:16.722] whiskey: java.lang.Exception: grrrr",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle2(LoggingTests.scala:21)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.LoggingTests$$anonfun$7.apply(LoggingTests.scala:133)",
                    "ERR [20080328-22:53:16.722] whiskey:     (...more...)",
                    "ERR [20080328-22:53:16.722] whiskey: Caused by java.lang.Exception: Aie!",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:10)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:12)",
                    "ERR [20080328-22:53:16.722] whiskey:     (...more...)")) {
            eat(handler.toString)
        }
    }
    
}
