package net.lag.logging

import java.text.SimpleDateFormat
import java.util.{Date, GregorianCalendar, TimeZone, logging => javalog}
import scala.collection.mutable
import net.lag.ConfiggyExtensions._


object Formatter {
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
 * Log entries are written in this format:
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
abstract class Formatter extends javalog.Formatter {

    var truncate_at: Int = 0
    var truncate_stack_traces_at: Int = 30
    private var _use_utc = false
    var calendar = new GregorianCalendar

    def use_utc = _use_utc

    def use_utc_=(utc: Boolean) = {
        _use_utc = utc
        if (utc) {
            // kind of ridiculous.
            calendar = new GregorianCalendar(TimeZone.getTimeZone("UTC"))
        } else {
            calendar = new GregorianCalendar
        }
        dateFormat.setCalendar(calendar)
    }

    def timeZone = calendar.getTimeZone.getDisplayName

    def timeZone_=(name: String) = {
        calendar = new GregorianCalendar(TimeZone.getTimeZone(name))
        dateFormat.setCalendar(calendar)
    }

    // implement me!
    def dateFormat: SimpleDateFormat

    // implement me!
    def lineTerminator: String

    // implement me!
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

        var message = record.getMessage
        if (record.getParameters != null) {
            message = String.format(message, record.getParameters)
        }
        if ((truncate_at > 0) && (message.length > truncate_at)) {
            message = message.substring(0, truncate_at) + "..."
        }

        // allow multi-line log entries to be atomic:
        var lines = new mutable.ArrayBuffer[String]
        lines ++= message.split("\n")

        if (record.getThrown != null) {
            lines += record.getThrown.toString
            lines ++= Formatter.formatStackTrace(record.getThrown, truncate_stack_traces_at)
        }
        val prefix = formatPrefix(record.getLevel, dateFormat.format(new Date(record.getMillis)), name)
        lines.mkString(prefix, lineTerminator + prefix, lineTerminator)
    }
}


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
