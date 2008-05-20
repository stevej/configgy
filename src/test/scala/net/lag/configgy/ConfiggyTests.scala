package net.lag.configgy

import java.io.{BufferedReader, InputStreamReader, File, FileInputStream, FileOutputStream}

import sorg.testing._

import net.lag.logging.{FileHandler, Logger}


object ConfiggyTests extends Tests {
    override def testName = "ConfiggyTests"
        

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
    
    
    test("load simple config file") {
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
            expect("Nibbler") { Configgy.config("name").get }
            Logger.get.info("this is at info level.")
            Logger.get.warning("this is at warning level.")
            
            // manually check that the loggers are correct:
            val root = Logger.get("")
            expect("WARNING") { root.getLevel.getName }
            expect(1) { root.getHandlers.length }
            expect("WARNING") { root.getHandlers()(0).getLevel.getName }
            expect(folderName + "/test.log") { root.getHandlers()(0).asInstanceOf[FileHandler].filename }

            // verify the logfile was configured and worked:
            // (reset forces the files to be flushed and closed.)
            Logger.reset
            val lines = readLogFile("test.log")
            expect(1) { lines.length }
            expect("WAR configgy: this is at warning level.") { lines(0) }
        }
    }
    
    test("load config file with multiple logfiles and levels") {
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
            expect(4) { lines.length }
            expect("WAR cat: message one") { lines(0) }
            expect("FAT cat: message two") { lines(1) }
            expect("FAT monkey: message four") { lines(2) }
            expect("DEB pig: message seven") { lines(3) }
            val lines2 = readLogFile("test2.log")
            expect(1) { lines2.length }
            expect("FAT monkey: message four") { lines2(0) }
        }
    }
    
    test("on-the-fly config changes are reflected in logging") {
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
            expect(3) { lines.length }
            expect("WAR cat: message two") { lines(0) }
            expect("INF cat: message three") { lines(1) }
            expect("WAR cat: message four") { lines(2) }
        }
    }
}
