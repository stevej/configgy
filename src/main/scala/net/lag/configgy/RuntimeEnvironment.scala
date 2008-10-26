/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

package net.lag.configgy

import java.io.File
import java.util.Properties
import net.lag.extensions._


/**
 * Use information in a local `build.properties` file to determine runtime
 * environment info like the package name, version, and installation path.
 * This can be used to automatically load config files from a `config/` path
 * relative to the executable jar.
 *
 * An example of how to generate a `build.properties` file is included in
 * configgy's ant files, and also in the "scala-build" github project here:
 * <http://github.com/robey/scala-build/tree/master>
 */
object RuntimeEnvironment {
  // load build info, if present.
  private var buildProperties = new Properties
  try {
    buildProperties.load(getClass.getResource("build.properties").openStream)
  } catch {
    case _ =>
  }

  val jarName = buildProperties.getProperty("name", "unknown")
  val jarVersion = buildProperties.getProperty("version", "0.0")
  val jarBuild = buildProperties.getProperty("build_name", "unknown")


  /**
   * Return the path this jar was executed from. Depends on the presence of
   * a valid `build.properties` file. Will return `None` if it couldn't
   * figure out the environment.
   */
  def getJarPath: Option[String] = {
    val pattern = ("(.*?)" + jarName + "-" + jarVersion + "\\.jar$").r
    val found = System.getProperty("java.class.path") split System.getProperty("path.separator") map {
      _ match {
        case pattern(path) => Some(new File(path).getCanonicalPath)
        case _ => None
      }
    } filter (_.isDefined)
    found.firstOption.flatMap(x => x)
  }

  /**
   * Config filename, as determined from this jar's runtime path, possibly
   * overridden by a command-line option.
   */
  var configFilename: String = getJarPath match {
    case Some(path) => path + "/config/" + jarName + ".conf"
    case None => "/etc/" + jarName + ".conf"
  }

  /**
   * Perform baseline command-line argument parsing. Responds to `--help`,
   * `--version`, and `-f` (which overrides the config filename).
   */
  def parseArgs(args: List[String]): Unit = {
    args match {
      case "-f" :: filename :: xs =>
        configFilename = filename
        parseArgs(xs)
      case "--help" :: xs =>
        help
      case "--version" :: xs =>
        println("%s %s (%s)".format(jarName, jarVersion, jarBuild))
      case unknown :: Nil =>
        println("Unknown command-line option: " + unknown)
        help
      case Nil =>
    }
  }

  private def help = {
    println
    println("%s %s (%s)".format(jarName, jarVersion, jarBuild))
    println("options:")
    println("    -f <filename>")
    println("        load config file (default: %s)".format(configFilename))
    println
    System.exit(0)
  }

  /**
   * Parse any command-line arguments (using `parseArgs`) and then load the
   * config file as determined by `configFilename` into the default config
   * block.
   */
  def load(args: Array[String]) = {
    parseArgs(args.toList)
    Configgy.configure(configFilename)
  }
}
