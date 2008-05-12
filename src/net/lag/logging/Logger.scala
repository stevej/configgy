package net.lag.logging

import java.text.SimpleDateFormat
import java.util.{Calendar, Date, logging => javalog}
import scala.collection.mutable
import net.lag.ConfiggyExtensions._
import net.lag.configgy.{AttributesException, AttributeMap}


// replace java's ridiculous log levels with the standard ones.
sealed case class Level(name: String, value: Int) extends javalog.Level(name, value) {
    Logger.levelNamesMap(name) = this
    Logger.levelsMap(value) = this
}
case object FATAL extends Level("FATAL", 1000)
case object CRITICAL extends Level("CRITICAL", 970)
case object ERROR extends Level("ERROR", 930)
case object WARNING extends Level("WARNING", 900)
case object INFO extends Level("INFO", 800)
case object DEBUG extends Level("DEBUG", 500)
case object TRACE extends Level("TRACE", 400)


class LoggingException(reason: String) extends Exception(reason)


class Logger private(val name: String, private val wrapped: javalog.Logger) {
    
    def log(level: Level, msg: String, items: Any*): Unit = log(level, null.asInstanceOf[Throwable], msg, items: _*)
    
    def log(level: Level, thrown: Throwable, msg: String, items: Any*): Unit = {
        val myLevel = getLevel
        if ((myLevel == null) || (level.intValue >= myLevel.intValue)) {
            val record = new javalog.LogRecord(level, msg)
            record.setParameters(items.toArray.asInstanceOf[Array[Object]])
            record.setLoggerName(wrapped.getName)
            if (thrown != null) {
                record.setThrown(thrown)
            }
            wrapped.log(record)
        }
    }
    
    // wrapped methods:
    def addHandler(handler: javalog.Handler) = wrapped.addHandler(handler)
    def getFilter() = wrapped.getFilter()
    def getHandlers() = wrapped.getHandlers()
    def getLevel() = wrapped.getLevel()
    def getParent() = wrapped.getParent()
    def getUseParentHandlers() = wrapped.getUseParentHandlers()
    def isLoggable(level: javalog.Level) = wrapped.isLoggable(level)
    def log(record: javalog.LogRecord) = wrapped.log(record)
    def removeHandler(handler: javalog.Handler) = wrapped.removeHandler(handler)
    def setFilter(filter: javalog.Filter) = wrapped.setFilter(filter)
    def setLevel(level: javalog.Level) = wrapped.setLevel(level)
    def setUseParentHandlers(use: Boolean) = wrapped.setUseParentHandlers(use)
    
    // convenience methods:
    def fatal(msg: String, items: Any*) = log(FATAL, msg, items: _*)
    def fatal(thrown: Throwable, msg: String, items: Any*) = log(FATAL, thrown, msg, items: _*)
    def critical(msg: String, items: Any*) = log(CRITICAL, msg, items: _*)
    def critical(thrown: Throwable, msg: String, items: Any*) = log(CRITICAL, thrown, msg, items: _*)
    def error(msg: String, items: Any*) = log(ERROR, msg, items: _*)
    def error(thrown: Throwable, msg: String, items: Any*) = log(ERROR, thrown, msg, items: _*)
    def warning(msg: String, items: Any*) = log(WARNING, msg, items: _*)
    def warning(thrown: Throwable, msg: String, items: Any*) = log(WARNING, thrown, msg, items: _*)
    def info(msg: String, items: Any*) = log(INFO, msg, items: _*)
    def info(thrown: Throwable, msg: String, items: Any*) = log(INFO, thrown, msg, items: _*)
    def debug(msg: String, items: Any*) = log(DEBUG, msg, items: _*)
    def debug(thrown: Throwable, msg: String, items: Any*) = log(DEBUG, thrown, msg, items: _*)
    def trace(msg: String, items: Any*) = log(TRACE, msg, items: _*)
    def trace(thrown: Throwable, msg: String, items: Any*) = log(TRACE, thrown, msg, items: _*)
    
    override def toString = {
        "<%s name='%s' level=%s handlers=%s use_parent=%s>".format(getClass.getName, name, getLevel(),
            getHandlers().toList.mkString("[", ", ", "]"), if (getUseParentHandlers()) "true" else "false")
    }
}


object Logger {
    private[logging] val levelNamesMap = new mutable.HashMap[String, Level]
    private[logging] val levelsMap = new mutable.HashMap[Int, Level]
    
    // cache scala Logger objects per name
    private val loggersCache = new mutable.HashMap[String, Logger]

    private val root = get("")
    
    // clear out some cruft from the java root logger.
    private val javaRoot = javalog.Logger.getLogger("")

    reset


    // convenience methods:
    def FATAL = logging.FATAL
    def CRITICAL = logging.CRITICAL
    def ERROR = logging.ERROR
    def WARNING = logging.WARNING
    def INFO = logging.INFO
    def DEBUG = logging.DEBUG
    def TRACE = logging.TRACE


    /**
     * Reset logging to an initial state, where all logging is set at
     * INFO level and goes to the console (stderr). Any existing log
     * handlers are removed.
     */
    def reset = {
        clearHandlers
        javaRoot.addHandler(new ConsoleHandler)
        root.setLevel(INFO)
    }

    def clearHandlers = {
        for (logger <- elements) {
            for (handler <- logger.getHandlers) {
                try {
                    handler.close()
                } catch { case _ => () }
                logger.removeHandler(handler)
            }
            logger.setLevel(null)
        }
    }
    
    def get(name: String): Logger = {
        loggersCache.get(name) match {
            case Some(logger) => logger
            case None => {
                val manager = javalog.LogManager.getLogManager
                val logger = manager.getLogger(name) match {
                    case null => {
                        val javaLogger = javalog.Logger.getLogger(name)
                        manager.addLogger(javaLogger)
                        new Logger(name, javaLogger)
                    }
                    case x: javalog.Logger => new Logger(name, x)
                }
                logger.setUseParentHandlers(true)
                loggersCache.put(name, logger)
                logger
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
            loggers += get(item.getName())
        }
        loggers.elements
    }
    
    /**
     * Create a Logger (or find an existing one) and configure it according
     * to a set of config keys.
     *
     * @param config a config block to parse
     * @param validateOnly don't actually configure the Logger, just throw an
     *     exception if the configuration is invalid
     * @param allowNestedBlocks consider the configuration valid if it
     *     contains nested config blocks, which are normally invalid
     *
     * @throws LoggingException if a config value can't be parsed correctly
     *     (some settings can only be one of a small possible set of values)
     */
    def configure(config: AttributeMap, validateOnly: Boolean, allowNestedBlocks: Boolean): Logger = {
        // make sure no other screwy attributes are in this AttributeMap
        val allowed = List("node", "console", "filename", "roll", "utc", "truncate", "truncate_stack_traces", "level", "use_parents")
        var forbidden = config.keys.filter(x => !(allowed contains x)).toList
        if (allowNestedBlocks) {
            forbidden = forbidden.filter(x => !config.getAttributes(x).isDefined)
        }
        if (forbidden.length > 0) {
            throw new LoggingException("Unknown logging config attribute(s): " + forbidden.mkString(", "))
        }
        
        val logger = Logger.get(config.get("node", ""))
        if (! validateOnly) {
            for (val handler <- logger.getHandlers) {
                logger.removeHandler(handler)
            }
        }
        
        var handlers: List[Handler] = Nil

        if (config.getBool("console", false)) {
            handlers = new ConsoleHandler :: handlers
        }
        
        // options for using a logfile
        for (val filename <- config.get("filename")) {
            // i bet there's an easier way to do this.
            val policy = config.get("roll", "never").toLowerCase match {
                case "never" => Never
                case "hourly" => Hourly
                case "daily" => Daily
                case "sunday" => Weekly(Calendar.SUNDAY)
                case "monday" => Weekly(Calendar.MONDAY)
                case "tuesday" => Weekly(Calendar.TUESDAY)
                case "wednesday" => Weekly(Calendar.WEDNESDAY)
                case "thursday" => Weekly(Calendar.THURSDAY)
                case "friday" => Weekly(Calendar.FRIDAY)
                case "saturday" => Weekly(Calendar.SATURDAY)
                case x => throw new LoggingException("Unknown logfile rolling policy: " + x)
            }
            handlers = new FileHandler(filename, policy) :: handlers
        }
        
        /* if they didn't specify a level, use "null", which is a secret
         * signal to javalog to use the parent logger's level. this is the
         * usual desired behavior, but not really documented anywhere. sigh.
         */
        val level = config.get("level") match {
            case Some(levelName) => {
                levelNamesMap.get(levelName.toUpperCase) match {
                    case Some(x) => x
                    case None => throw new LoggingException("Unknown log level: " + levelName)
                }
            }
            case None => null
        }

        for (val handler <- handlers) {
            handler.setLevel(level)
            handler.use_utc = config.getBool("utc", false)
            handler.truncate_at = config.getInt("truncate", 0)
            handler.truncate_stack_traces_at = config.getInt("truncate_stack_traces", 30)
            if (! validateOnly) {
                logger.addHandler(handler)
            }
        }

        if (! validateOnly) {
            logger.setUseParentHandlers(config.getBool("use_parents", true))
            logger.setLevel(level)
        }
        
        logger
    }
}
