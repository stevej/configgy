package net.lag.configgy

import java.io.File
import net.lag.logging.Logger


/**
 * Main API entry point into the configgy library.
 */
object Configgy {
    private var _config: Config = null
    private val subscriber = new LoggingConfigSubscriber

    /**
     * The base Config object for this server. This will only be defined
     * after calling one of <code>configure</code> or
     * <code>configureFromResource</code>.
     */
    def config = _config

    // remember the previous path/filename we loaded, for reload().
    private var previousPath: String = null
    private var previousFilename: String = null

    /**
     * Configure the server by loading a config file from the given path
     * and filename. The filename must be relative to the path. The path is
     * used to resolve filenames given in "include" lines.
     */
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

        previousPath = path
        previousFilename = filename
    }

    /**
     * Configure the server by loading a config file from the given filename.
     * The base folder will be extracted from the filename and used as a base
     * path for resolving filenames given in "include" lines.
     */
    def configure(filename: String): Unit = {
        val n = filename.lastIndexOf('/')
        if (n < 0) {
            configure(new File(".").getCanonicalPath, filename)
        } else {
            configure(filename.substring(0, n), filename.substring(n + 1))
        }
    }

    /**
     * Reload the previously-loaded config file from disk. Any changes will
     * take effect immediately. <b>All</b> subscribers will be called to
     * verify and commit the change (even if their nodes didn't actually
     * change).
     */
    def reload: Unit = {
        try {
            _config.loadFile(previousPath, previousFilename)
        } catch {
            case e: Throwable => {
                Logger.get.critical(e, "Failed to reload config file '%s/%s'", previousPath, previousFilename)
                throw e
            }
        }
    }

    /**
     * Configure the server by loading a config file from the given named
     * resource inside this jar file. "include" lines will also operate
     * on resource paths.
     */
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
