/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

package net.lag.configgy

import java.io.File
import scala.collection.{Map, Set}
import scala.collection.{immutable, mutable}
import net.lag.extensions._
import net.lag.logging.Logger


private abstract class Phase
private case object VALIDATE_PHASE extends Phase
private case object COMMIT_PHASE extends Phase


private class SubscriptionNode {
  var subscribers = new mutable.HashSet[Subscriber]
  var map = new mutable.HashMap[String, SubscriptionNode]

  def get(name: String): SubscriptionNode = {
    map.get(name) match {
      case Some(x) => x
      case None =>
        val node = new SubscriptionNode
        map(name) = node
        node
    }
  }

  override def toString() = {
    val out = new StringBuilder("%d" format subscribers.size)
    if (map.size > 0) {
      out.append(" { ")
      for (val key <- map.keys) {
        out.append(key)
        out.append("=")
        out.append(map(key).toString)
        out.append(" ")
      }
      out.append("}")
    }
    out.toString
  }

  @throws(classOf[ValidationException])
  def validate(key: List[String], current: Option[ConfigMap], replacement: Option[ConfigMap], phase: Phase): Unit = {
    if ((current == None) && (replacement == None)) {
      // someone has subscribed to a nonexistent node... ignore.
      return
    }

    // first, call all subscribers for this node.
    for (val subscriber <- subscribers) {
      phase match {
        case VALIDATE_PHASE => subscriber.validate(current, replacement)
        case COMMIT_PHASE => subscriber.commit(current, replacement)
      }
    }

    /* if we're walking a key, lookup the next segment's subscribers and
     * continue the validate/commit. if the key is exhausted, call
     * subscribers for ALL nodes below this one.
     */
    var nextNodes: Iterator[(String, SubscriptionNode)] = null
    key match {
      case Nil => nextNodes = map.elements
      case segment :: _ => {
        map.get(segment) match {
            case None => return     // done!
            case Some(node) => nextNodes = Iterator.single((segment, node))
        }
      }
    }

    for (val (segment, node) <- nextNodes) {
      val subCurrent = current match {
        case None => None
        case Some(x) => x.getConfigMap(segment)
      }
      val subReplacement = replacement match {
        case None => None
        case Some(x) => x.getConfigMap(segment)
      }
      node.validate(if (key == Nil) Nil else key.tail, subCurrent, subReplacement, phase)
    }
  }
}


/**
 * An attribute map of key/value pairs and subscriptions, where values may
 * be other attribute maps. Config objects represent the "root" of a nested
 * set of attribute maps, and control the flow of subscriptions and events
 * for subscribers.
 */
class Config extends ConfigMap {
  private var root = new Attributes(this, "")
  private val subscribers = new SubscriptionNode
  private val subscriberKeys = new mutable.HashMap[Int, (SubscriptionNode, Subscriber)]
  private var nextKey = 1

  /**
   * Importer for resolving "include" lines when loading config files.
   * By default, it's a FilesystemImporter based on the current working
   * directory.
   */
  var importer: Importer = new FilesystemImporter(new File(".").getCanonicalPath)


  /**
   * Read config data from a string and use it to populate this object.
   */
  def load(data: String) = {
    if (root.isMonitored) {
      // reload: swap, validate, and replace.
      var newRoot = new Attributes(this, "")
      new ConfigParser(newRoot, importer).parse(data)

      // throws exception if validation fails:
      subscribers.validate(Nil, Some(root), Some(newRoot), VALIDATE_PHASE)
      subscribers.validate(Nil, Some(root), Some(newRoot), COMMIT_PHASE)

      root = newRoot
    } else {
      new ConfigParser(root, importer).parse(data)
    }
    root.setMonitored
  }

  /**
   * Read config data from a file and use it to populate this object.
   */
  def loadFile(filename: String) = {
    load(importer.importFile(filename))
  }

  /**
   * Read config data from a file and use it to populate this object.
   */
  def loadFile(path: String, filename: String) = {
    importer = new FilesystemImporter(path)
    load(importer.importFile(filename))
  }


  override def toString = root.toString


  // -----  subscriptions

  private[configgy] def subscribe(key: String, subscriber: Subscriber): SubscriptionKey = synchronized {
    root.setMonitored
    var subkey = nextKey
    nextKey += 1
    var node = subscribers
    if (key != null) {
      for (val segment <- key.split("\\.")) {
        node = node.get(segment)
      }
    }
    node.subscribers += subscriber
    subscriberKeys += Pair(subkey, (node, subscriber))
    new SubscriptionKey(this, subkey)
  }

  private[configgy] def subscribe(key: String)(f: (Option[ConfigMap]) => Unit): SubscriptionKey = {
    subscribe(key, new Subscriber {
      def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
      def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = {
        f(replacement)
      }
    })
  }

  def subscribe(subscriber: Subscriber) = subscribe(null.asInstanceOf[String], subscriber)

  override def subscribe(f: (Option[ConfigMap]) => Unit) = subscribe(null.asInstanceOf[String])(f)

  private[configgy] def unsubscribe(subkey: SubscriptionKey) = synchronized {
    subscriberKeys.get(subkey.id) match {
      case None => false
      case Some((node, sub)) => {
        node.subscribers -= sub
        subscriberKeys -= subkey.id
        true
      }
    }
  }

  /**
   * Return a formatted string of all the subscribers, useful for debugging.
   */
  def debugSubscribers() = synchronized {
    "subs=" + subscribers.toString
  }


  // -----  modifications that happen within monitored Attributes nodes

  @throws(classOf[ValidationException])
  private def deepChange(name: String, key: String, operation: (ConfigMap, String) => Boolean): Boolean = synchronized {
    val fullKey = if (name == "") (key) else (name + "." + key)
    val newRoot = root.copy
    val keyList = fullKey.split("\\.").toList

    if (! operation(newRoot, fullKey)) {
      return false
    }

    // throws exception if validation fails:
    subscribers.validate(keyList, Some(root), Some(newRoot), VALIDATE_PHASE)
    subscribers.validate(keyList, Some(root), Some(newRoot), COMMIT_PHASE)

    newRoot.setMonitored
    root = newRoot
    true
  }

  private[configgy] def deepSet(name: String, key: String, value: String) = {
    deepChange(name, key, { (newRoot, fullKey) => newRoot(fullKey) = value; true })
  }

  private[configgy] def deepSet(name: String, key: String, value: Seq[String]) = {
    deepChange(name, key, { (newRoot, fullKey) => newRoot(fullKey) = value; true })
  }

  private[configgy] def deepRemove(name: String, key: String): Boolean = {
    deepChange(name, key, { (newRoot, fullKey) => newRoot.remove(fullKey) })
  }


  // -----  implement AttributeMap by wrapping our root object:

  def getString(key: String): Option[String] = root.getString(key)
  def getConfigMap(key: String): Option[ConfigMap] = root.getConfigMap(key)
  def configMap(key: String): ConfigMap = root.configMap(key)
  def getList(key: String): Seq[String] = root.getList(key)
  def setString(key: String, value: String): Unit = root.setString(key, value)
  def setList(key: String, value: Seq[String]): Unit = root.setList(key, value)
  def contains(key: String): Boolean = root.contains(key)
  def remove(key: String): Boolean = root.remove(key)
  def keys: Iterator[String] = root.keys
  def asMap(): Map[String, String] = root.asMap
}


object Config {
  /**
   * Create a config object from a config file of the given path
   * and filename. The filename must be relative to the path. The path is
   * used to resolve filenames given in "include" lines.
   */
  def fromFile(path: String, filename: String): Config = {
    val config = new Config
    try {
      config.loadFile(path, filename)
    } catch {
      case e: Throwable =>
        Logger.get.critical(e, "Failed to load config file '%s/%s'", path, filename)
        throw e
    }
    config
  }

  /**
   * Create a Config object from a config file of the given filename.
   * The base folder will be extracted from the filename and used as a base
   * path for resolving filenames given in "include" lines.
   */
  def fromFile(filename: String): Config = {
    val n = filename.lastIndexOf('/')
    if (n < 0) {
      fromFile(new File(".").getCanonicalPath, filename)
    } else {
      fromFile(filename.substring(0, n), filename.substring(n + 1))
    }
  }

  /**
   * Create a Config object from the given named
   * resource inside this jar file. "include" lines will also operate
   * on resource paths.
   */
  def fromResource(name: String) = {
    val config = new Config
    try {
      config.importer = new ResourceImporter
      config.loadFile(name)
    } catch {
      case e: Throwable =>
        Logger.get.critical(e, "Failed to load config resource '%s'", name)
        throw e
    }
    config
  }

  /**
   * Create a Config object from a map of String keys and String values.
   */
  def fromMap(m: Map[String, String]) = {
    val config = new Config
    for ((k, v) <- m.elements) {
      config(k) = v
    }
    config
  }
}
