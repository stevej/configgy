/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

package net.lag.logging

import java.text.SimpleDateFormat
import java.util.{Calendar, Date, logging => javalog}
import scala.collection.Map
import scala.collection.mutable
import net.lag.extensions._
import net.lag.configgy.{ConfigException, ConfigMap}


// replace java's ridiculous log levels with the standard ones.
sealed case class Level(name: String, value: Int) extends javalog.Level(name, value) {
  Logger.levelNamesMap(name) = this
  Logger.levelsMap(value) = this
}
case object OFF extends Level("OFF", Math.MAX_INT)
case object FATAL extends Level("FATAL", 1000)
case object CRITICAL extends Level("CRITICAL", 970)
case object ERROR extends Level("ERROR", 930)
case object WARNING extends Level("WARNING", 900)
case object INFO extends Level("INFO", 800)
case object DEBUG extends Level("DEBUG", 500)
case object TRACE extends Level("TRACE", 400)
case object ALL extends Level("ALL", Math.MIN_INT)


class LoggingException(reason: String) extends Exception(reason)


private[logging] class LazyLogRecord(level: javalog.Level, messageGenerator: => AnyRef) extends javalog.LogRecord(level, "") {
  // for each logged line, generate this string only once, regardless of how many handlers there are:
  var cached: Option[AnyRef] = None

  def generate = {
    cached match {
      case Some(value) =>
        value
      case None =>
        cached = Some(messageGenerator)
        cached.get
    }
  }
}


/**
 * Scala wrapper for logging.
 */
class Logger private(val name: String, private val wrapped: javalog.Logger) {
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

  override def toString = {
    "<%s name='%s' level=%s handlers=%s use_parent=%s>".format(getClass.getName, name, getLevel(),
      getHandlers().toList.mkString("[", ", ", "]"), if (getUseParentHandlers()) "true" else "false")
  }

  /**
   * Log a message, with sprintf formatting, at the desired level.
   */
  def log(level: Level, msg: String, items: Any*): Unit = log(level, null.asInstanceOf[Throwable], msg, items: _*)

  /**
   * Log a message, with sprintf formatting, at the desired level, and
   * attach an exception and stack trace.
   */
  def log(level: Level, thrown: Throwable, message: String, items: Any*): Unit = {
    val myLevel = getLevel
    if ((myLevel == null) || (level.intValue >= myLevel.intValue)) {
      val record = new javalog.LogRecord(level, message)
      record.setParameters(items.toArray.asInstanceOf[Array[Object]])
      record.setLoggerName(wrapped.getName)
      if (thrown != null) {
        record.setThrown(thrown)
      }
      wrapped.log(record)
    }
  }

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

  /**
   * Log a message, with lazy (call-by-name) computation of the message,
   * at the desired level.
   */
  def logLazy(level: Level, message: => AnyRef): Unit = logLazy(level, null.asInstanceOf[Throwable], message)

  /**
   * Log a message, with lazy (call-by-name) computation of the message,
   * and attach an exception and stack trace.
   */
  def logLazy(level: Level, thrown: Throwable, message: => AnyRef): Unit = {
    val myLevel = getLevel
    if ((myLevel == null) || (level.intValue >= myLevel.intValue)) {
      val record = new LazyLogRecord(level, message)
      record.setLoggerName(wrapped.getName)
      if (thrown != null) {
        record.setThrown(thrown)
      }
      wrapped.log(record)
    }
  }

  // convenience methods:
  def ifFatal(message: => AnyRef) = logLazy(FATAL, message)
  def ifFatal(thrown: Throwable, message: => AnyRef) = logLazy(FATAL, thrown, message)
  def ifCritical(message: => AnyRef) = logLazy(CRITICAL, message)
  def ifCritical(thrown: Throwable, message: => AnyRef) = logLazy(CRITICAL, thrown, message)
  def ifError(message: => AnyRef) = logLazy(ERROR, message)
  def ifError(thrown: Throwable, message: => AnyRef) = logLazy(ERROR, thrown, message)
  def ifWarning(message: => AnyRef) = logLazy(WARNING, message)
  def ifWarning(thrown: Throwable, message: => AnyRef) = logLazy(WARNING, thrown, message)
  def ifInfo(message: => AnyRef) = logLazy(INFO, message)
  def ifInfo(thrown: Throwable, message: => AnyRef) = logLazy(INFO, thrown, message)
  def ifDebug(message: => AnyRef) = logLazy(DEBUG, message)
  def ifDebug(thrown: Throwable, message: => AnyRef) = logLazy(DEBUG, thrown, message)
  def ifTrace(message: => AnyRef) = logLazy(TRACE, message)
  def ifTrace(thrown: Throwable, message: => AnyRef) = logLazy(TRACE, thrown, message)
}


object Logger {
  private[logging] val levelNamesMap = new mutable.HashMap[String, Level]
  private[logging] val levelsMap = new mutable.HashMap[Int, Level]

  // cache scala Logger objects per name
  private val loggersCache = new mutable.HashMap[String, Logger]

  private val root: Logger = get("")

  // clear out some cruft from the java root logger.
  private val javaRoot = javalog.Logger.getLogger("")


  // ----- convenience methods:

  /** OFF is used to turn off logging entirely. */
  def OFF = logging.OFF

  /** Describes an event which will cause the application to exit immediately, in failure. */
  def FATAL = logging.FATAL

  /** Describes an event which will cause the application to fail to work correctly, but
   *  keep attempt to continue. The application may be unusable.
   */
  def CRITICAL = logging.CRITICAL

  /** Describes a user-visible error that may be transient or not affect other users. */
  def ERROR = logging.ERROR

  /** Describes a problem which is probably not user-visible but is notable and/or may be
   *  an early indication of a future error.
   */
  def WARNING = logging.WARNING

  /** Describes information about the normal, functioning state of the application. */
  def INFO = logging.INFO

  /** Describes information useful for general debugging, but probably not normal,
   *  day-to-day use.
   */
  def DEBUG = logging.DEBUG

  /** Describes information useful for intense debugging. */
  def TRACE = logging.TRACE

  /** ALL is used to log everything. */
  def ALL = logging.ALL

  // to force them to get loaded from class files:
  root.setLevel(FATAL)
  root.setLevel(CRITICAL)
  root.setLevel(ERROR)
  root.setLevel(WARNING)
  root.setLevel(INFO)
  root.setLevel(DEBUG)
  root.setLevel(TRACE)
  reset


  /**
   * Return a map of log level values to the corresponding Level objects.
   */
  def levels: Map[Int, Level] = levelsMap.readOnly

  /**
   * Return a map of log level names to the corresponding Level objects.
   */
  def levelNames: Map[String, Level] = levelNamesMap.readOnly

  /**
   * Reset logging to an initial state, where all logging is set at
   * INFO level and goes to the console (stderr). Any existing log
   * handlers are removed.
   */
  def reset = {
    clearHandlers
    javaRoot.addHandler(new ConsoleHandler(new FileFormatter))
    root.setLevel(INFO)
  }

  /**
   * Remove all existing log handlers from all existing loggers.
   */
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

  /**
   * Return a logger for the given package name. If one doesn't already
   * exist, a new logger will be created and returned.
   */
  def get(name: String): Logger = {
    loggersCache.get(name) match {
      case Some(logger) =>
        logger
      case None =>
        val manager = javalog.LogManager.getLogManager
        val logger = manager.getLogger(name) match {
          case null =>
            val javaLogger = javalog.Logger.getLogger(name)
            manager.addLogger(javaLogger)
            new Logger(name, javaLogger)
          case x: javalog.Logger =>
            new Logger(name, x)
        }
        logger.setUseParentHandlers(true)
        loggersCache.put(name, logger)
        logger
    }
  }

  /** An alias for <code>get(name)</code> */
  def apply(name: String) = get(name)

  /**
   * Return a logger for the class name of the class/object that called
   * this method. Normally you would use this in a "private val"
   * declaration on the class/object. The class name is determined
   * by sniffing around on the stack.
   */
  def get: Logger = {
    val className = new Throwable().getStackTrace()(1).getClassName
    if (className.endsWith("$")) {
      get(className.substring(0, className.length - 1))
    } else {
      get(className)
    }
  }

  /** An alias for <code>get()</code> */
  def apply() = get

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
  def configure(config: ConfigMap, validateOnly: Boolean, allowNestedBlocks: Boolean): Logger = {
    // make sure no other screwy attributes are in this AttributeMap
    val allowed = List("node", "console", "filename", "roll", "utc",
                       "truncate", "truncate_stack_traces", "level",
                       "use_parents", "syslog_host", "syslog_server_name",
                       "syslog_use_iso_date_format")
    var forbidden = config.keys.filter(x => !(allowed contains x)).toList
    if (allowNestedBlocks) {
      forbidden = forbidden.filter(x => !config.getConfigMap(x).isDefined)
    }
    if (forbidden.length > 0) {
      throw new LoggingException("Unknown logging config attribute(s): " + forbidden.mkString(", "))
    }

    val logger = Logger.get(config.getString("node", ""))
    if (! validateOnly) {
      for (val handler <- logger.getHandlers) {
        logger.removeHandler(handler)
      }
    }

    var handlers: List[Handler] = Nil

    if (config.getBool("console", false)) {
      handlers = new ConsoleHandler(new FileFormatter) :: handlers
    }

    for (val hostname <- config.getString("syslog_host")) {
      val useIsoDateFormat = config.getBool("syslog_use_iso_date_format", true)
      val handler = new SyslogHandler(useIsoDateFormat, hostname)
      for (val serverName <- config.getString("syslog_server_name")) {
        handler.serverName = serverName
      }
      handlers = handler :: handlers
    }

    // options for using a logfile
    for (val filename <- config.getString("filename")) {
      // i bet there's an easier way to do this.
      val policy = config.getString("roll", "never").toLowerCase match {
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
      handlers = new FileHandler(filename, policy, new FileFormatter) :: handlers
    }

    /* if they didn't specify a level, use "null", which is a secret
     * signal to javalog to use the parent logger's level. this is the
     * usual desired behavior, but not really documented anywhere. sigh.
     */
    val level = config.getString("level") match {
      case Some(levelName) => {
        levelNamesMap.get(levelName.toUpperCase) match {
          case Some(x) => x
          case None => throw new LoggingException("Unknown log level: " + levelName)
        }
      }
      case None => null
    }

    for (val handler <- handlers) {
      if (level != null) {
        handler.setLevel(level)
      }
      handler.useUtc = config.getBool("utc", false)
      handler.truncateAt = config.getInt("truncate", 0)
      handler.truncateStackTracesAt = config.getInt("truncate_stack_traces", 30)
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
