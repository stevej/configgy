package net.lag.logging

import java.text.SimpleDateFormat
import java.util.{Date, GregorianCalendar, TimeZone, logging => javalog}
import scala.collection.mutable
import net.lag.extensions._


private[logging] object Formatter {
  // FIXME: might be nice to unmangle some scala names here.
  private[logging] def formatStackTrace(t: Throwable, limit: Int): mutable.ArrayBuffer[String] = {
    var out = new mutable.ArrayBuffer[String]
    out ++= (for (elem <- t.getStackTrace) yield "    at %s".format(elem.toString))
    if (out.length > limit) {
      out = new mutable.ArrayBuffer[String] ++ out.take(limit)
      out += "    (...more...)"
    }
    if (t.getCause != null) {
      out += "Caused by %s".format(t.getCause.toString)
      out ++= formatStackTrace(t.getCause, limit)
    }
    out
  }
}


/**
 * A standard log formatter for scala. This extends the java built-in
 * log formatter.
 *
 * Truncation, exception formatting, multi-line logging, and time zones
 * are handled in this class. Subclasses are called for formatting the
 * line prefix, formatting the date, and determining the line terminator.
 *
 */
abstract class Formatter extends javalog.Formatter {

  /**
   * Where to truncate log messages (character count). 0 = don't truncate.
   */
  var truncateAt: Int = 0

  /**
   * Where to truncate stack traces in exception logging (line count).
   */
  var truncateStackTracesAt: Int = 30

  private var _useUtc = false

  /**
   * Calendar to use for time zone display in date-time formatting.
   */
  var calendar = new GregorianCalendar

  /**
   * Return <code>true</code> if dates in log messages are being reported
   * in UTC time, or <code>false</code> if they're being reported in local
   * time.
   */
  def useUtc = _useUtc

  /**
   * Set whether dates in log messages should be reported in UTC time
   * (<code>true</code>) or local time (<code>false</code>, the default).
   * This variable and <code>timeZone</code> affect the same settings, so
   * whichever is called last will take precedence.
   */
  def useUtc_=(utc: Boolean) = {
    _useUtc = utc
    if (utc) {
      // kind of ridiculous.
      calendar = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
    } else {
      calendar = new GregorianCalendar
    }
    dateFormat.setCalendar(calendar)
  }

  /**
   * Return the name of the time zone currently used for formatting dates
   * in log messages. Normally this will either be the local time zone or
   * UTC (if <code>use_utc</code> was set), but it can also be set
   * manually.
   */
  def timeZone = calendar.getTimeZone.getDisplayName

  /**
   * Set the time zone for formatting dates in log messages. The time zone
   * name must be one known by the java <code>TimeZone</code> class.
   */
  def timeZone_=(name: String) = {
    calendar = new GregorianCalendar(TimeZone.getTimeZone(name))
    dateFormat.setCalendar(calendar)
  }

  /**
   * Return the date formatter to use for log messages.
   */
  def dateFormat: SimpleDateFormat

  /**
   * Return the line terminator (if any) to use at the end of each log
   * message.
   */
  def lineTerminator: String

  /**
   * Return the string to prefix each log message with, given a log level,
   * formatted date string, and package name.
   */
  def formatPrefix(level: javalog.Level, date: String, name: String): String

  override def format(record: javalog.LogRecord): String = {
    val name = record.getLoggerName match {
      case "" => "(root)"
      case n => {
        val nameSegments = n.split("\\.")
        if (nameSegments.length >= 2) {
          nameSegments(nameSegments.length - 2)
        } else {
          n
        }
      }
    }

    var message: String = record match {
      case r: LazyLogRecord =>
        r.generate.toString
      case r: javalog.LogRecord =>
        r.getParameters match {
          case null =>
            r.getMessage
          case formatArgs =>
            String.format(r.getMessage, formatArgs)
        }
    }

    if ((truncateAt > 0) && (message.length > truncateAt)) {
      message = message.substring(0, truncateAt) + "..."
    }

    // allow multi-line log entries to be atomic:
    var lines = new mutable.ArrayBuffer[String]
    lines ++= message.split("\n")

    if (record.getThrown != null) {
      lines += record.getThrown.toString
      lines ++= Formatter.formatStackTrace(record.getThrown, truncateStackTracesAt)
    }
    val prefix = formatPrefix(record.getLevel, dateFormat.format(new Date(record.getMillis)), name)
    lines.mkString(prefix, lineTerminator + prefix, lineTerminator)
  }
}


/**
 * The standard log formatter for a logfile. Log entries are written in this format:
 *
 * <pre>
 *     ERR [20080315-18:39:05.033] julius: et tu, brute?
 * </pre>
 *
 * which indicates the level (error), the date/time, the logger's name
 * (julius), and the message. The logger's name is usually also the
 * last significant segment of the package name (ie "com.lag.julius"),
 * although packages can override this.
 */
class FileFormatter extends Formatter {
  private val DATE_FORMAT = new SimpleDateFormat("yyyyMMdd-HH:mm:ss.SSS")

  override def dateFormat = DATE_FORMAT
  override def lineTerminator = "\n"

  override def formatPrefix(level: javalog.Level, date: String, name: String): String = {
    val levelName = level match {
      case Level(name, _) => name.substring(0, 3)
      case x: javalog.Level => {
        // if it maps to one of our levels, use our name.
        Logger.levelsMap.get(x.intValue) match {
          case None => "%03d".format(x.intValue)
          case Some(level) => level.name.substring(0, 3)
        }
      }
    }

    "%s [%s] %s: ".format(levelName, date, name)
  }
}
