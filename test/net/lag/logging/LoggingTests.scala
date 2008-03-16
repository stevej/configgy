package net.lag.logging

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


object LoggingTests extends Tests {
    
    override def testName = "LoggingTests"
    
    private var handler = new StringHandler

    override def setUp = {
        Logger.clearHandlers
        handler = new StringHandler
        Logger.get("").addHandler(handler)
    }
    
    override def tearDown = {
        Logger.clearHandlers
    }
    
        
    // turn logged console lines into a list of repeatable strings (date & time removed)
    private def eat(in: String) = {
        val lines = in.split("\n")
        var out: List[String] = Nil
        for (x <- lines) {
            out = x.substring(0, 5) + "20080315-01:23:45.999" + x.substring(26) :: out
        }
        out.reverse
    }
        
    test("simple") {
        val log = Logger.get("")
        log.error("error!")
        expect(List("ERR [20080315-01:23:45.999] scala: error!")) { eat(handler.toString) }
    }
    
    test("packages") {
        val log1 = Logger.get("net.lag.logging.Skeletor")
        log1.warning("I am coming for you!")
        val log2 = Logger.get("net.lag.configgy.Skeletor")
        log2.warning("I am also coming for you!")
        
        expect(List("WAR [20080315-01:23:45.999] logging: I am coming for you!",
                    "WAR [20080315-01:23:45.999] configgy: I am also coming for you!")) {
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
        
        expect(List("WAR [20080315-01:23:45.999] logging: I am coming for you!",
                    "DEB [20080315-01:23:45.999] logging: Loading supplies...",
                    "ERR [20080315-01:23:45.999] logging: Help!")) {
            eat(handler.toString)
        }
    }
    
    test("truncate") {
        handler.truncate_at = 30
        val log1 = Logger.get("net.lag.whiskey.Train")
        log1.critical("Something terrible happened that may take a very long time to explain because I write crappy log messages.")
        
        expect(List("CRI [20080315-01:23:45.999] whiskey: Something terrible happened th...")) {
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
        
        expect(List("ERR [20080315-01:23:45.999] whiskey: Exception!",
                    "ERR [20080315-01:23:45.999] whiskey: java.lang.Exception: Aie!",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:9)",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:11)",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:11)",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:11)",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:11)",
                    "ERR [20080315-01:23:45.999] whiskey:     (...more...)")) {
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
        
        expect(List("ERR [20080315-01:23:45.999] whiskey: Exception!",
                    "ERR [20080315-01:23:45.999] whiskey: java.lang.Exception: grrrr",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle2(LoggingTests.scala:20)",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.LoggingTests$$anonfun$6.apply(LoggingTests.scala:121)",
                    "ERR [20080315-01:23:45.999] whiskey:     (...more...)",
                    "ERR [20080315-01:23:45.999] whiskey: Caused by java.lang.Exception: Aie!",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:9)",
                    "ERR [20080315-01:23:45.999] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:11)",
                    "ERR [20080315-01:23:45.999] whiskey:     (...more...)")) {
            eat(handler.toString)
        }
    }
}
