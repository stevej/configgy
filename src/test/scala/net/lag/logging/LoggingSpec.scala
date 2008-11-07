/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

package net.lag.logging

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.net.{DatagramPacket, DatagramSocket, InetSocketAddress}
import java.util.{Date, logging => javalog}
import org.specs._
import net.lag.configgy.Config
import net.lag.extensions._


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


class TimeWarpingStringHandler extends StringHandler(new FileFormatter) {
  formatter.timeZone = "GMT-7"

  override def publish(record: javalog.LogRecord) = {
    record.setMillis(1206769996722L)
    super.publish(record)
  }
}


class TimeWarpingSyslogHandler(useIsoDateFormat: Boolean, server: String) extends SyslogHandler(useIsoDateFormat, server) {
  formatter.timeZone = "GMT-7"

  override def publish(record: javalog.LogRecord) = {
    record.setMillis(1206769996722L)
    super.publish(record)
  }

  getFormatter.asInstanceOf[SyslogFormatter].hostname = "raccoon.local"
}


class ImmediatelyRollingFileHandler(filename: String, policy: Policy) extends FileHandler(filename, policy, new FileFormatter) {
  formatter.timeZone = "GMT-7"

  override def computeNextRollTime(): Long = System.currentTimeMillis + 100

  override def publish(record: javalog.LogRecord) = {
    record.setMillis(1206769996722L)
    super.publish(record)
  }
}


object LoggingSpec extends Specification with TestHelper {

  private var handler: Handler = null

  // turn logged console lines into a list of repeatable strings
  private def eat(in: String): List[String] = {
    for (val line <- in.split("\n").toList) yield {
      line.regexSub("""LoggingSpec.scala:\d+""".r) { m => "LoggingSpec.scala:NNN" }
    }.regexSub("""LoggingSpec\$[\w\$]+""".r) {
      m => "LoggingSpec$$"
    }
  }


  "Logging" should {
    doBefore {
      Logger.clearHandlers
      handler = new TimeWarpingStringHandler
      Logger.get("").addHandler(handler)
    }

    doAfter {
      Logger.clearHandlers
    }

    "provide level name and value maps" in {
      Logger.levels mustEqual Map(TRACE.value -> TRACE, DEBUG.value -> DEBUG,
        INFO.value -> INFO, WARNING.value -> WARNING, ERROR.value -> ERROR,
        CRITICAL.value -> CRITICAL, FATAL.value -> FATAL)
      Logger.levelNames mustEqual Map("TRACE" -> TRACE, "DEBUG" -> DEBUG,
        "INFO" -> INFO, "WARNING" -> WARNING, "ERROR" -> ERROR,
        "CRITICAL" -> CRITICAL, "FATAL" -> FATAL)
    }

    "perform basic logging" in {
      val log = Logger("")
      log.error("error!")
      eat(handler.toString) mustEqual List("ERR [20080328-22:53:16.722] (root): error!")
    }

    "do lazy message evaluation" in {
      val log = Logger.get("")
      var callCount = 0
      def getSideEffect = {
        callCount += 1
        "ok"
      }
      // add 2nd handler:
      log.addHandler(new TimeWarpingStringHandler)
      log.ifError("this is " + getSideEffect)
      // should not generate since it's not handled:
      log.ifDebug("this is not " + getSideEffect)

      eat(handler.toString) mustEqual List("ERR [20080328-22:53:16.722] (root): this is ok")
      // verify that the string was generated exactly once, even tho we logged it to 2 handlers:
      callCount mustEqual 1
    }

    // verify that we can ask logs to be written in UTC
    "log in utc when asked to" in {
      val log = Logger.get("")
      log.getHandlers()(0).asInstanceOf[Handler].useUtc = true
      log.error("error!")
      eat(handler.toString) mustEqual List("ERR [20080329-05:53:16.722] (root): error!")
    }

    "log package names" in {
      val log1 = Logger.get("net.lag.logging.Skeletor")
      log1.warning("I am coming for you!")
      val log2 = Logger.get("net.lag.configgy.Skeletor")
      log2.warning("I am also coming for you!")

      eat(handler.toString) mustEqual
        List("WAR [20080328-22:53:16.722] logging: I am coming for you!",
             "WAR [20080328-22:53:16.722] configgy: I am also coming for you!")

      handler.asInstanceOf[StringHandler].clear
      handler.formatter.useFullPackageNames = true
      log1.warning("I am coming for you!")
      log2.warning("I am also coming for you!")
      eat(handler.toString) mustEqual
        List("WAR [20080328-22:53:16.722] net.lag.logging: I am coming for you!",
             "WAR [20080328-22:53:16.722] net.lag.configgy: I am also coming for you!")
    }

    "log level names" in {
      val log1 = Logger.get("net.lag.logging.Skeletor")
      log1.setLevel(DEBUG)
      log1.warning("I am coming for you!")
      log1.debug("Loading supplies...")
      log1.trace("Catfood query.")
      log1.error("Help!")

      eat(handler.toString) mustEqual
        List("WAR [20080328-22:53:16.722] logging: I am coming for you!",
             "DEB [20080328-22:53:16.722] logging: Loading supplies...",
             "ERR [20080328-22:53:16.722] logging: Help!")
    }

    "truncate lines" in {
      handler.truncateAt = 30
      val log1 = Logger.get("net.lag.whiskey.Train")
      log1.critical("Something terrible happened that may take a very long time to explain because I write crappy log messages.")

      eat(handler.toString) mustEqual
        List("CRI [20080328-22:53:16.722] whiskey: Something terrible happened th...")
    }

    "write stack traces" in {
      handler.truncateStackTracesAt = 5
      val log1 = Logger.get("net.lag.whiskey.Train")
      try {
        Crazy.cycle(10)
      } catch {
        case t: Throwable => log1.error(t, "Exception!")
      }

      eat(handler.toString) mustEqual
        List("ERR [20080328-22:53:16.722] whiskey: Exception!",
             "ERR [20080328-22:53:16.722] whiskey: java.lang.Exception: Aie!",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     (...more...)")
    }

    "write nested stack traces" in {
      handler.truncateStackTracesAt = 2
      val log1 = Logger.get("net.lag.whiskey.Train")
      try {
        Crazy.cycle2(2)
      } catch {
        case t: Throwable => log1.error(t, "Exception!")
      }

      eat(handler.toString) mustEqual
        List("ERR [20080328-22:53:16.722] whiskey: Exception!",
             "ERR [20080328-22:53:16.722] whiskey: java.lang.Exception: grrrr",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle2(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.LoggingSpec$$.apply(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     (...more...)",
             "ERR [20080328-22:53:16.722] whiskey: Caused by java.lang.Exception: Aie!",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     at net.lag.logging.Crazy$.cycle(LoggingSpec.scala:NNN)",
             "ERR [20080328-22:53:16.722] whiskey:     (...more...)")
    }


    "roll logs on time" in {
      val rollHandler = new FileHandler("/tmp/test.log", Hourly, new FileFormatter)
      rollHandler.computeNextRollTime(1206769996722L) mustEqual 1206770400000L
      rollHandler.computeNextRollTime(1206770400000L) mustEqual 1206774000000L
      rollHandler.computeNextRollTime(1206774000001L) mustEqual 1206777600000L
    }

    // verify that at the proper time, the log file rolls and resets.
    "roll logs into new files" in {
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
        new BufferedReader(new InputStreamReader(new FileInputStream(movedFilename), "UTF-8")).readLine mustEqual
          "FAT [20080328-22:53:16.722] whiskey: first file."
        new BufferedReader(new InputStreamReader(new FileInputStream(folderName + "/test.log"), "UTF-8")).readLine mustEqual
          "FAT [20080328-22:53:16.722] whiskey: second file."
      }
    }

    "write syslog entries" in {
      // start up new syslog listener
      val serverSocket = new DatagramSocket
      val serverPort = serverSocket.getLocalPort

      var syslog = new TimeWarpingSyslogHandler(true, "localhost:" + serverPort)
      val log = Logger.get("net.lag.whiskey.Train")
      log.addHandler(syslog)
      log.setLevel(DEBUG)

      log.fatal("fatal message!")
      log.error("error message!")
      syslog.serverName = "pingd"
      log.warning("warning message!")
      syslog.clearServerName
      log.debug("and debug!")

      val p = new DatagramPacket(new Array[Byte](1024), 1024)
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<9>2008-03-28T22:53:16 raccoon.local whiskey: fatal message!"
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<11>2008-03-28T22:53:16 raccoon.local whiskey: error message!"
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<12>2008-03-28T22:53:16 raccoon.local [pingd] whiskey: warning message!"
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<15>2008-03-28T22:53:16 raccoon.local whiskey: and debug!"

      log.removeHandler(syslog)
      syslog = new TimeWarpingSyslogHandler(false, "localhost:" + serverPort)
      log.addHandler(syslog)
      log.info("here's an info message with BSD time.")
      serverSocket.receive(p)
      new String(p.getData, 0, p.getLength) mustEqual "<14>Mar 28 22:53:16 raccoon.local whiskey: here's an info message with BSD time."
    }


    "configure logging" in {
      withTempFolder {
        val TEST_DATA =
          "node=\"net.lag\"\n" +
          "filename=\"" + folderName + "/test.log\"\n" +
          "level=\"debug\"\n" +
          "truncate=1024\n"

        val c = new Config
        c.load(TEST_DATA)
        val log = Logger.configure(c, false, false)

        log.getLevel mustEqual DEBUG
        log.getHandlers.length mustEqual 1
        val h = log.getHandlers()(0)
        h.asInstanceOf[FileHandler].filename mustEqual folderName + "/test.log"
        log.name mustEqual "net.lag"
        h.asInstanceOf[Handler].truncateAt mustEqual 1024
      }

      withTempFolder {
        val TEST_DATA =
          "node=\"net.lag\"\n" +
          "syslog_host=\"example.com:212\"\n" +
          "syslog_server_name=\"elmo\"\n"

        val c = new Config
        c.load(TEST_DATA)
        val log = Logger.configure(c, false, false)

        log.getHandlers.length mustEqual 1
        val h = log.getHandlers()(0)
        h.isInstanceOf[SyslogHandler] mustEqual true
        h.asInstanceOf[SyslogHandler].dest.asInstanceOf[InetSocketAddress].getHostName mustEqual "example.com"
        h.asInstanceOf[SyslogHandler].dest.asInstanceOf[InetSocketAddress].getPort mustEqual 212
        h.asInstanceOf[SyslogHandler].serverName mustEqual "elmo"
      }
    }

    "handle config errors" in {
      // should throw an exception because of the unknown attribute
      val TEST_DATA =
        "filename=\"foobar.log\"\n" +
        "level=\"debug\"\n" +
        "style=\"html\"\n"

      val c = new Config
      c.load(TEST_DATA)
      Logger.configure(c, false, false) must throwA(new LoggingException(""))
    }
  }
}
