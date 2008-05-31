package net.lag.configgy

import java.io.File
import net.lag.logging.Logger


object Configgy {
    private var _config: Config = null
    private val subscriber = new LoggingConfigSubscriber

    def config = _config


    def configure(path: String, filename: String): Unit = {
        Logger.reset

        _config = new Config
        try {
            _config.loadFile(path, filename)
        } catch {
            case e: Throwable => {
                Logger.get.critical(e, "Failed to load config file '%s/%s'", path, filename)
                throw e
            }
        }

        configLogging
    }

    def configure(filename: String): Unit = {
        val n = filename.lastIndexOf('/')
        if (n < 0) {
            configure(new File(".").getCanonicalPath, filename)
        } else {
            configure(filename.substring(0, n), filename.substring(n + 1))
        }
    }

    def configureFromResource(name: String) = {
        Logger.reset

        _config = new Config
        try {
            _config.importer = new ResourceImporter
            _config.loadFile(name)
        } catch {
            case e: Throwable => {
                Logger.get.critical(e, "Failed to load config resource '%s'", name)
                throw e
            }
        }

        configLogging
    }

    private def configLogging = {
        val log = Logger.get("")

        try {
            val attr = _config.getAttributes("log")
            subscriber.commit(None, attr)
            if (attr.isDefined) {
                attr.get.subscribe(subscriber)
            }
        } catch {
            case e: Throwable => {
                log.critical(e, "Failed to configure logging")
                throw e
            }
        }
    }


    private class LoggingConfigSubscriber extends Subscriber {
        @throws(classOf[ValidationException])
        def validate(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit = {
            try {
                for (val logConfig <- replacement) {
                    Logger.configure(logConfig, true, true)
                    for (val key <- logConfig.keys if logConfig.getAttributes(key).isDefined) {
                        Logger.configure(logConfig.getAttributes(key).get, true, false)
                    }
                }
            } catch {
                case e: Throwable => throw new ValidationException(e.toString)
            }
        }

        def commit(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit = {
            Logger.reset

            for (val logConfig <- replacement) {
                Logger.configure(logConfig, false, true)
                for (val key <- logConfig.keys if logConfig.getAttributes(key).isDefined) {
                    Logger.configure(logConfig.getAttributes(key).get, false, false)
                }
            }
        }
    }
}
