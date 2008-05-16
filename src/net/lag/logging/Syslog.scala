package net.lag.logging

import java.util.{logging => javalog}
import java.net.{DatagramPacket, DatagramSocket, InetAddress, InetSocketAddress, SocketAddress}
import java.text.SimpleDateFormat
import net.lag.ConfiggyExtensions._


object Syslog {
    val DEFAULT_PORT = 514

    val PRIORITY_USER = 8
    val PRIORITY_DAEMON = 24
    val PRIORITY_LOCAL0 = 128
    val PRIORITY_LOCAL1 = 136
    val PRIORITY_LOCAL2 = 144
    val PRIORITY_LOCAL3 = 152
    val PRIORITY_LOCAL4 = 160
    val PRIORITY_LOCAL5 = 168
    val PRIORITY_LOCAL6 = 176
    val PRIORITY_LOCAL7 = 184

    private val SEVERITY_EMERGENCY = 0
    private val SEVERITY_ALERT = 1
    private val SEVERITY_CRITICAL = 2
    private val SEVERITY_ERROR = 3
    private val SEVERITY_WARNING = 4
    private val SEVERITY_NOTICE = 5
    private val SEVERITY_INFO = 6
    private val SEVERITY_DEBUG = 7

    /**
     * Convert the java/scala log level into its closest syslog-ng severity.
     */
    private[logging] def severityForLogLevel(level: Int): Int = {
        if (level >= FATAL.value) {
            SEVERITY_ALERT
        } else if (level >= CRITICAL.value) {
            SEVERITY_CRITICAL
        } else if (level >= ERROR.value) {
            SEVERITY_ERROR
        } else if (level >= WARNING.value) {
            SEVERITY_WARNING
        } else if (level >= INFO.value) {
            SEVERITY_INFO
        } else {
            SEVERITY_DEBUG
        }
    }
}


class SyslogFormatter(useIsoDateFormat: Boolean) extends Formatter {
    private val ISO_DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
    private val OLD_SYSLOG_DATE_FORMAT = new SimpleDateFormat("MMM dd HH:mm:ss")

    // user may override:
    def priority = Syslog.PRIORITY_USER

    // user may override:
    def hostname = InetAddress.getLocalHost().getHostName()

    override def dateFormat = if (useIsoDateFormat) ISO_DATE_FORMAT else OLD_SYSLOG_DATE_FORMAT
    override def lineTerminator = ""

    override def formatPrefix(level: javalog.Level, date: String, name: String): String = {
        val syslogLevel = level match {
            case Level(name, x) => Syslog.severityForLogLevel(x)
            case x: javalog.Level => Syslog.severityForLogLevel(x.intValue)
        }
        "<%d>%s %s %s: ".format(priority | syslogLevel, date, hostname, name)
    }
}


class SyslogHandler(useIsoDateFormat: Boolean, server: String) extends Handler(new SyslogFormatter(useIsoDateFormat)) {
    private val socket = new DatagramSocket
    private val dest: SocketAddress = server.split(":", 2).toList match {
        case host :: port :: _ => new InetSocketAddress(host, port.toInt)
        case host :: _ => new InetSocketAddress(host, Syslog.DEFAULT_PORT)
        case Nil => null
    }

    def flush() = { }
    def close() = { }

    def publish(record: javalog.LogRecord) = synchronized {
        try {
            val data = getFormatter.format(record).getBytes
            val packet = new DatagramPacket(data, data.length, dest)
            socket.send(packet)
        } catch {
            case e => {
                System.err.println(Formatter.formatStackTrace(e, 30).mkString("\n"))
            }
        }
    }
}
