/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

package net.lag.logging

import java.io.{File, FileOutputStream, OutputStreamWriter, Writer}
import java.text.SimpleDateFormat
import java.util.{Calendar, Date, logging => javalog}


sealed abstract class Policy
case object Never extends Policy
case object Hourly extends Policy
case object Daily extends Policy
case class Weekly(dayOfWeek: Int) extends Policy


/**
 * A log handler that writes log entries into a file, and rolls this file
 * at a requested interval (hourly, daily, or weekly).
 */
class FileHandler(val filename: String, val policy: Policy, formatter: Formatter,
                  val append: Boolean) extends Handler(formatter) {

  private var stream: Writer = null
  private var openTime: Long = 0
  private var nextRollTime: Long = 0
  openLog()


  def flush() = {
    stream.flush()
  }

  def close() = {
    flush()
    try {
      stream.close()
    } catch { case _ => () }
  }

  private def openLog() = {
    stream = new OutputStreamWriter(new FileOutputStream(filename, append), "UTF-8")
    openTime = System.currentTimeMillis
    nextRollTime = computeNextRollTime()
  }

  /**
   * Compute the suffix for a rolled logfile, based on the roll policy.
   */
  def timeSuffix(date: Date) = {
    val dateFormat = policy match {
      case Never => new SimpleDateFormat("yyyy")
      case Hourly => new SimpleDateFormat("yyyyMMdd-HH")
      case Daily => new SimpleDateFormat("yyyyMMdd")
      case Weekly(_) => new SimpleDateFormat("yyyyMMdd")
    }
    dateFormat.setCalendar(formatter.calendar)
    dateFormat.format(date)
  }

  /**
   * Return the time (in absolute milliseconds) of the next desired
   * logfile roll.
   */
  def computeNextRollTime(now: Long): Long = {
    val next = formatter.calendar.clone.asInstanceOf[Calendar]
    next.setTimeInMillis(now)
    next.set(Calendar.MILLISECOND, 0)
    next.set(Calendar.SECOND, 0)
    next.set(Calendar.MINUTE, 0)
    policy match {
      case Never =>
        next.add(Calendar.YEAR, 100)
      case Hourly =>
        next.add(Calendar.HOUR_OF_DAY, 1)
      case Daily =>
        next.set(Calendar.HOUR_OF_DAY, 0)
        next.add(Calendar.DAY_OF_MONTH, 1)
      case Weekly(weekday) =>
        next.set(Calendar.HOUR_OF_DAY, 0)
        while (next.get(Calendar.DAY_OF_WEEK) != weekday) {
            next.add(Calendar.DAY_OF_MONTH, 1)
        }
    }
    next.getTimeInMillis
  }

  def computeNextRollTime(): Long = computeNextRollTime(System.currentTimeMillis)

  private def roll() = {
    stream.close()
    val n = filename.lastIndexOf('.')
    var newFilename = if (n > 0) {
      filename.substring(0, n) + "-" + timeSuffix(new Date(openTime)) + filename.substring(n)
    } else {
      filename + "-" + timeSuffix(new Date(openTime))
    }
    new File(filename).renameTo(new File(newFilename))
    openLog()
  }

  def publish(record: javalog.LogRecord) = synchronized {
    try {
      if (System.currentTimeMillis > nextRollTime) {
        roll
      }
      stream.write(getFormatter.format(record))
      stream.flush
    } catch {
      case e =>
        System.err.println(Formatter.formatStackTrace(e, 30).mkString("\n"))
    }
  }
}
