package net.lag.logging

import java.text.SimpleDateFormat
import java.util.{Date, logging => javalog}
import scala.collection.mutable

import net.lag.configgy.StringUtils


// replace java's ridiculous log levels with the standard ones.
sealed case class Level(name: String, value: Int) extends javalog.Level(name, value)
case object FATAL extends Level("FATAL", 1000)
case object CRITICAL extends Level("CRITICAL", 970)
case object ERROR extends Level("ERROR", 930)
case object WARNING extends Level("WARNING", 900)
case object INFO extends Level("INFO", 800)
case object DEBUG extends Level("DEBUG", 500)
case object TRACE extends Level("TRACE", 400)


class Logger private(name: String) extends javalog.Logger(name, null) {
    
    def log(level: Level, msg: String, items: Any*): Unit = log(level, null.asInstanceOf[Throwable], msg, items: _*)
    
    def log(level: Level, thrown: Throwable, msg: String, items: Any*): Unit = {
        val myLevel = getLevel
        if ((myLevel == null) || (level.intValue >= myLevel.intValue)) {
            val record = new javalog.LogRecord(level, msg)
            record.setParameters(items.toArray.asInstanceOf[Array[Object]])
            record.setLoggerName(getName)
            if (thrown != null) {
                record.setThrown(thrown)
            }
            log(record)
        }
    }
    
    // convenience methods:
    def fatal(msg: String, items: Any*) = log(FATAL, msg, items: _*)
    def fatal(thrown: Throwable, msg: String, items: Any*) = log(FATAL, thrown, msg, items)
    def critical(msg: String, items: Any*) = log(CRITICAL, msg, items: _*)
    def critical(thrown: Throwable, msg: String, items: Any*) = log(CRITICAL, thrown, msg, items)
    def error(msg: String, items: Any*) = log(ERROR, msg, items: _*)
    def error(thrown: Throwable, msg: String, items: Any*) = log(ERROR, thrown, msg, items)
    def warning(msg: String, items: Any*) = log(WARNING, msg, items: _*)
    def warning(thrown: Throwable, msg: String, items: Any*) = log(WARNING, thrown, msg, items)
    def info(msg: String, items: Any*) = log(INFO, msg, items: _*)
    def info(thrown: Throwable, msg: String, items: Any*) = log(INFO, thrown, msg, items)
    def debug(msg: String, items: Any*) = log(DEBUG, msg, items: _*)
    def debug(thrown: Throwable, msg: String, items: Any*) = log(DEBUG, thrown, msg, items)
    def trace(msg: String, items: Any*) = log(TRACE, msg, items: _*)
    def trace(thrown: Throwable, msg: String, items: Any*) = log(TRACE, thrown, msg, items)
    
    // these are needed to override java evilness:
    def fatal(msg: String) = log(FATAL, "%s", msg)
    def critical(msg: String) = log(CRITICAL, "%s", msg)
    def error(msg: String) = log(ERROR, "%s", msg)
    override def warning(msg: String) = log(WARNING, "%s", msg)
    override def info(msg: String) = log(INFO, "%s", msg)
    def debug(msg: String) = log(DEBUG, "%s", msg)
    def trace(msg: String) = log(TRACE, "%s", msg)
}


object Logger {
    
    private val root = get("")
    root.setLevel(WARNING)
    
    // clear out some cruft from the java root logger.
    private val javaRoot = javalog.Logger.getLogger("")
    clearHandlers
    
    // log to console at first
    javaRoot.addHandler(new ConsoleHandler)


    def get(name: String): Logger = {
        // all scala loggers will be under "scala."
        val fullName = if (name == "") "scala" else ("scala." + name)
        
        val manager = javalog.LogManager.getLogManager
        manager.getLogger(fullName) match {
            case null => {
                val logger = new Logger(fullName)
                logger.setUseParentHandlers(true)
                manager.addLogger(logger)
                logger
            }
            case x: Logger => x
            case crufty => {
                throw new RuntimeException("BAD, someone snuck cruft into the scala logging tree")
            }
        }
    }
    
    def get: Logger = {
        val className = new Throwable().getStackTrace()(1).getClassName
        if (className.endsWith("$")) {
            get(className.substring(0, className.length - 1))
        } else {
            get(className)
        }
    }
    
    /**
     * Iterate the Logger objects that have been created.
     */
    def elements: Iterator[Logger] = {
        val manager = javalog.LogManager.getLogManager
        val loggers = new mutable.Queue[Logger]
        // why on earth did java use ENUMERATION here?!
        val e = manager.getLoggerNames
        while (e.hasMoreElements) {
            val item = manager.getLogger(e.nextElement.asInstanceOf[String])
            if (item.isInstanceOf[Logger]) {
                loggers += item.asInstanceOf[Logger]
            }
        }
        loggers.elements
    }
    
    def clearHandlers = {
        for (handler <- javaRoot.getHandlers) {
            javaRoot.removeHandler(handler)
        }
        for (logger <- elements) {
            for (handler <- logger.getHandlers) {
                logger.removeHandler(handler)
            }
        }
    }

}
