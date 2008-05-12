package net.lag.logging

import java.util.{logging => javalog}
import scala.collection.mutable
import net.lag.ConfiggyExtensions._


abstract class Handler extends javalog.Handler {
    private var formatter = new Formatter

    setFormatter(formatter)
    

    def truncate_at = formatter.truncate_at
    
    def truncate_at_=(n: Int) = {
        formatter.truncate_at = n
    }
    
    def truncate_stack_traces_at = formatter.truncate_stack_traces_at
    
    def truncate_stack_traces_at_=(n: Int) = {
        formatter.truncate_stack_traces_at = n
    }
    
    def use_utc = getFormatter.asInstanceOf[Formatter].use_utc
    def use_utc_=(utc: Boolean) = getFormatter.asInstanceOf[Formatter].use_utc = utc
    def calendar = getFormatter.asInstanceOf[Formatter].calendar
    
    override def toString = {
        "<%s level=%s utc=%s truncate=%d truncate_stack=%d>".format(getClass.getName, getLevel,
            if (use_utc) "true" else "false", truncate_at, truncate_stack_traces_at)
    }
}


/**
 * Mostly useful for unit tests: logging goes directly into a
 * string buffer.
 */
class StringHandler extends Handler {
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
class ConsoleHandler extends Handler {
    def publish(record: javalog.LogRecord) = {
        System.err.print(getFormatter().format(record))
    }
    
    def close() = { }
    
    def flush() = Console.flush
}
