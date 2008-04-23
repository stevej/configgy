package net.lag.logging

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.util.{Date, logging => javalog}
import sorg.testing._

import net.lag.configgy.Config


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


class ImmediatelyRollingFileHandler(filename: String, policy: Policy) extends FileHandler(filename, policy) {
    override def computeNextRollTime(): Long = System.currentTimeMillis + 100

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
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:13)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:15)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:15)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:15)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:15)",
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
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle2(LoggingTests.scala:24)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.LoggingTests$$anonfun$7.apply(LoggingTests.scala:146)",
                    "ERR [20080328-22:53:16.722] whiskey:     (...more...)",
                    "ERR [20080328-22:53:16.722] whiskey: Caused by java.lang.Exception: Aie!",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:13)",
                    "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingTests.scala:15)",
                    "ERR [20080328-22:53:16.722] whiskey:     (...more...)")) {
            eat(handler.toString)
        }
    }
    
    test("log roll timing") {
        val rollHandler = new FileHandler("/tmp/test.log", Hourly)
        expect(1206770400000L) {
            rollHandler.computeNextRollTime(1206769996722L)
        }
        expect(1206774000000L) {
            rollHandler.computeNextRollTime(1206770400000L)
        }
        expect(1206777600000L) {
            rollHandler.computeNextRollTime(1206774000001L)
        }
    }
    
    // verify that at the proper time, the log file rolls and resets.
    test("log roll") {
        withTempFolder {
            val rollHandler = new ImmediatelyRollingFileHandler(folderName + "/test.log", Hourly)
            val log = Logger.get("net.lag.whiskey.Train")
            val date = new Date()
            log.addHandler(rollHandler)
            log.fatal("first file.")
            
            Thread.sleep(150)
            
            log.fatal("second file.")
            rollHandler.close()
            
            val movedFilename = folderName + "/test-" + rollHandler.timeSuffix(date) + ".log"
            expect("FAT [20080328-22:53:16.722] whiskey: first file.") {
                new BufferedReader(new InputStreamReader(new FileInputStream(movedFilename), "UTF-8")).readLine
            }
            expect("FAT [20080328-22:53:16.722] whiskey: second file.") {
                new BufferedReader(new InputStreamReader(new FileInputStream(folderName + "/test.log"), "UTF-8")).readLine
            }
        }
    }
    
        
    test("log config") {
        withTempFolder {
            val TEST_DATA =
                "node=\"net.lag\"\n" +
                "filename=\"" + folderName + "/test.log\"\n" +
                "level=\"debug\"\n" +
                "truncate=1024\n"

            val c = new Config
            c.load(TEST_DATA)
            val log = Logger.configure(c, false, false)
            
            expect(DEBUG) { log.getLevel }
            expect(1) { log.getHandlers.length }
            val h = log.getHandlers()(0)
            expect(folderName + "/test.log") { h.asInstanceOf[FileHandler].filename }
            expect("net.lag") { log.name }
            expect(1024) { h.asInstanceOf[Handler].truncate_at }
        }
    }
    
    test("log config errors") {
        // should throw an exception because of the unknown attribute
        val TEST_DATA =
            "filename=\"foobar.log\"\n" +
            "level=\"debug\"\n" +
            "style=\"html\"\n"
        
        val c = new Config
        c.load(TEST_DATA)
        expectThrow(classOf[LoggingException]) {
            Logger.configure(c, false, false)
        }
    }
}
