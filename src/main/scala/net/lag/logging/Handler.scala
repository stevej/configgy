package net.lag.logging

import java.util.{logging => javalog}
import scala.collection.mutable
import net.lag.extensions._


/**
 * A base log handler for scala. This extends the java built-in handler
 * and connects it with a formatter automatically.
 */
abstract class Handler(_formatter: Formatter) extends javalog.Handler {

    setFormatter(_formatter)


    /**
     * Where to truncate log messages (character count). 0 = don't truncate.
     */
    def truncateAt = formatter.truncateAt

    /**
     * Where to truncate log messages (character count). 0 = don't truncate.
     */
    def truncateAt_=(n: Int) = {
        formatter.truncateAt = n
    }

    /**
     * Where to truncate stack traces in exception logging (line count).
     */
    def truncateStackTracesAt = formatter.truncateStackTracesAt

    /**
     * Where to truncate stack traces in exception logging (line count).
     */
    def truncateStackTracesAt_=(n: Int) = {
        formatter.truncateStackTracesAt = n
    }

    /**
     * Return <code>true</code> if dates in log messages are being reported
     * in UTC time, or <code>false</code> if they're being reported in local
     * time.
     */
    def useUtc = formatter.useUtc

    /**
     * Set whether dates in log messages should be reported in UTC time
     * (<code>true</code>) or local time (<code>false</code>, the default).
     * This variable and <code>timeZone</code> affect the same settings, so
     * whichever is called last will take precedence.
     */
    def useUtc_=(utc: Boolean) = formatter.useUtc = utc

    /**
     * Return the formatter associated with this log handler.
     */
    def formatter = getFormatter.asInstanceOf[Formatter]

    override def toString = {
        "<%s level=%s utc=%s truncate=%d truncate_stack=%d>".format(getClass.getName, getLevel,
            if (useUtc) "true" else "false", truncateAt, truncateStackTracesAt)
    }
}


/**
 * Mostly useful for unit tests: logging goes directly into a
 * string buffer.
 */
class StringHandler(_formatter: Formatter) extends Handler(_formatter) {
    private var buffer = new StringBuilder()

    def publish(record: javalog.LogRecord) = {
        buffer append getFormatter().format(record)
    }

    def close() = { }

    def flush() = { }

    override def toString = buffer.toString
}


/**
 * Log things to the console.
 */
class ConsoleHandler(_formatter: Formatter) extends Handler(_formatter) {
    def publish(record: javalog.LogRecord) = {
        System.err.print(getFormatter().format(record))
    }

    def close() = { }

    def flush() = Console.flush
}
