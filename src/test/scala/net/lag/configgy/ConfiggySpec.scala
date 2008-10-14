package net.lag.configgy

import java.io.{BufferedReader, InputStreamReader, File, FileInputStream, FileOutputStream}
import net.lag.logging.{FileHandler, Logger}
import org.specs._


object ConfiggySpec extends Specification with TestHelper {

  private def writeConfigFile(filename: String, data: String) = {
    val f = new FileOutputStream(folderName + "/" + filename)
    f.write(data.getBytes)
    f.close
  }

  private def readLogFile(filename: String) = {
    val f = new BufferedReader(new InputStreamReader(new FileInputStream(folderName + "/" + filename)))
    var lines: List[String] = Nil
    var line = f.readLine
    while (line != null) {
      lines = (line.substring(0, 4) + line.substring(28)) :: lines
      line = f.readLine
    }
    lines.reverse.toArray
  }


  "Configgy" should {
    "load a simple config file" in {
      withTempFolder {
        val data1 =
          "name=\"Nibbler\"\n" +
          "\n" +
          "<log>\n" +
          "    filename=\"" + folderName + "/test.log\"\n" +
          "    level=\"WARNING\"\n" +
          "</log>\n"
        writeConfigFile("test.conf", data1)

        Configgy.configure(folderName, "test.conf")

        // verify the config file got loaded:
        Configgy.config("name") mustEqual "Nibbler"

        Logger.get.info("this is at info level.")
        Logger.get.warning("this is at warning level.")

        // manually check that the loggers are correct:
        val root = Logger.get("")
        root.getLevel.getName mustEqual "WARNING"
        root.getHandlers.length mustEqual 1
        root.getHandlers()(0).getLevel.getName mustEqual "WARNING"
        root.getHandlers()(0).asInstanceOf[FileHandler].filename mustEqual(folderName + "/test.log")

        // verify the logfile was configured and worked:
        // (reset forces the files to be flushed and closed.)
        Logger.reset
        val lines = readLogFile("test.log")
        lines.length mustEqual 1
        lines(0) mustEqual "WAR configgy: this is at warning level."
      }
    }

    "load a config file with multiple logfiles and levels" in {
      withTempFolder {
        val data1 =
          "name=\"Fry\"\n" +
          "\n" +
          "<log>\n" +
          "    filename=\"" + folderName + "/test.log\"\n" +
          "    level=\"WARNING\"\n" +
          "\n" +
          "    <aux>\n" +
          "        filename = \"" + folderName + "/test2.log\"\n" +
          "        level=\"FATAL\"\n" +
          "        node=\"net.lag.monkey\"\n" +
          "    </aux>\n" +
          "\n" +
          "    <troublesome>\n" +
          "        node=\"net.lag.pig\"\n" +
          "        level=\"DEBUG\"\n" +
          "    </troublesome>\n" +
          "</log>\n"
        writeConfigFile("test.conf", data1)

        Configgy.configure(folderName, "test.conf")

        // test.log should get any warnings, but test2.log should only get fatals from monkey.
        Logger.get("net.lag.cat.1").warning("message one")
        Logger.get("net.lag.cat.1").fatal("message two")
        Logger.get("net.lag.monkey.1").error("message three")
        Logger.get("net.lag.monkey.1").fatal("message four")

        // debug messages should be logged to test.log, but only from pig.
        Logger.get("net.lag.cat.1").debug("message five")
        Logger.get("net.lag.monkey.1").debug("message six")
        Logger.get("net.lag.pig.1").debug("message seven")

        Logger.reset
        val lines = readLogFile("test.log")
        lines.length mustEqual 4
        lines(0) mustEqual "WAR cat: message one"
        lines(1) mustEqual "FAT cat: message two"
        lines(2) mustEqual "FAT monkey: message four"
        lines(3) mustEqual "DEB pig: message seven"
        val lines2 = readLogFile("test2.log")
        lines2.length mustEqual 1
        lines2(0) mustEqual "FAT monkey: message four"
      }
    }

    "reflect on-the-fly config changes in logging" in {
      withTempFolder {
        val data1 =
          "name=\"Fry\"\n" +
          "\n" +
          "<log>\n" +
          "    filename=\"" + folderName + "/test.log\"\n" +
          "    level=\"WARNING\"\n" +
          "</log>\n"
        writeConfigFile("test.conf", data1)

        Configgy.configure(folderName, "test.conf")

        // test.log should only get the warning for this one:
        Logger.get("net.lag.cat.1").info("message one")
        Logger.get("net.lag.cat.1").warning("message two")

        Configgy.config("log.level") = "debug"

        // test.log should get both of these:
        Logger.get("net.lag.cat.1").info("message three")
        Logger.get("net.lag.cat.1").warning("message four")

        Logger.reset
        val lines = readLogFile("test.log")
        lines.length mustEqual 3
        lines(0) mustEqual "WAR cat: message two"
        lines(1) mustEqual "INF cat: message three"
        lines(2) mustEqual "WAR cat: message four"
      }
    }

    "reload" in {
      withTempFolder {
        val data1 =
          "<robot>\n" +
          "    name=\"Nibbler\"\n" +
          "    age = 23002\n" +
          "</robot>\n" +
          "<unchanged>\n" +
          "    stuff = 0\n"
          "</unchanged>\n"
        writeConfigFile("test.conf", data1)
        Configgy.configure(folderName, "test.conf")

        Configgy.config.getInt("robot.age", 0) mustEqual 23002

        var checked = false
        var checkedAlso = false
        Configgy.config.subscribe("robot") { (attr: Option[ConfigMap]) => checked = true }
        Configgy.config.subscribe("unchanged") { (attr: Option[ConfigMap]) => checkedAlso = true }
        checked mustBe false

        val data2 =
          "<robot>\n" +
          "    name=\"Nibbler\"\n" +
          "    age = 23003\n" +
          "</robot>\n" +
          "<unchanged>\n" +
          "    stuff = 0\n"
          "</unchanged>\n"
        writeConfigFile("test.conf", data2)

        Configgy.reload

        // all subscribers (even for unchanged nodes) should be called.
        checked mustBe true
        checkedAlso mustBe true
        Configgy.config.getInt("robot.age", 0) mustEqual 23003
      }
    }
  }
}
