package net.lag.logging

import java.text.SimpleDateFormat
import java.util.{Date, logging => javalog}
import scala.collection.mutable

import net.lag.configgy.StringUtils


/**
 * A standard log formatter for scala.
 *
 * Log entries are written in this format::
 *
 *     ERR [20080315-18:39:05.033] julius: et tu, brute?
 *
 * which indicates the level (error), the date/time, the logger's name
 * (julius), and the message. The logger's name is usually also the
 * last significant segment of the package name (ie "com.lag.julius"),
 * although packages can override this.
 */
class Formatter extends javalog.Formatter {
    
    var truncate_at: Int = 0
    var truncate_stack_traces_at: Int = 30
    
    private val DATE_FORMAT = new SimpleDateFormat("yyyyMMdd-HH:mm:ss.SSS")
    
    override def format(record: javalog.LogRecord): String = {
        val level = record.getLevel match {
            case Level(name, _) => name.substring(0, 3)
            case _ => "???"
        }
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
            lines ++= formatStackTrace(record.getThrown)
        }
        val prefix = StringUtils.format("%s [%s] %s: ", level, DATE_FORMAT.format(new Date(record.getMillis)), name)
        lines.mkString(prefix, "\n" + prefix, "\n")
    }
    
    // FIXME: might be nice to unmangle some scala names here.
    private def formatStackTrace(t: Throwable): mutable.ArrayBuffer[String] = {
        var out = new mutable.ArrayBuffer[String]
        out ++= (for (elem <- t.getStackTrace) yield StringUtils.format("    at %s", elem.toString))
        if (out.length > truncate_stack_traces_at) {
            out = new mutable.ArrayBuffer[String] ++ out.take(truncate_stack_traces_at)
            out += "    (...more...)"
        }
        if (t.getCause != null) {
            out += StringUtils.format("Caused by %s", t.getCause.toString)
            out ++= formatStackTrace(t.getCause)
        }
        out
    }
    
}
