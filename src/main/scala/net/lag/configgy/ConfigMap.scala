/*
 * Copyright (c) 2008, Robey Pointer <robeypointer@gmail.com>
 * ISC licensed. Please see the included LICENSE file for more information.
 */

package net.lag.configgy

import scala.collection.Map
import scala.util.Sorting


class ConfigException(reason: String) extends Exception(reason)


/**
 * Abstract trait for a map of string keys to strings, string lists, or (nested) ConfigMaps.
 * Integers and booleans may also be stored and retrieved, but they are converted to/from
 * strings in the process.
 */
trait ConfigMap {
  private val TRUE = "true"
  private val FALSE = "false"


  // -----  required methods

  /**
   * Lookup an entry in this map, and if it exists and can be represented
   * as a string, return it. Strings will be returned as-is, and string
   * lists will be returned as a combined string. Nested AttributeMaps
   * will return None as if there was no entry present.
   */
  def getString(key: String): Option[String]

  /**
   * Lookup an entry in this map, and if it exists and is a nested
   * ConfigMap, return it. If the entry is a string or string list,
   * it will return None as if there was no entry present.
   */
  def getConfigMap(key: String): Option[ConfigMap]

  /**
   * Lookup an entry in this map, and if it exists and is a nested
   * ConfigMap, return it. If not, create an empty map with this name
   * and return that.
   *
   * @throws ConfigException if the key already refers to a string or
   *     string list
   */
  def configMap(key: String): ConfigMap

  /**
   * Lookup an entry in this map, and if it exists and can be represented
   * as a string list, return it. String lists will be returned as-is, and
   * strings will be returned as an array of length 1. Nested ConfigMaps
   * will return None as if there was no entry present.
   */
  def getList(key: String): Seq[String]

  /**
   * Set a key/value pair in this map. If an entry already existed with
   * that key, it's replaced.
   *
   * @throws ConfigException if the key already refers to a nested
   *     AttributeMap
   */
  def setString(key: String, value: String): Unit

  /**
   * Set a key/value pair in this map. If an entry already existed with
   * that key, it's replaced.
   *
   * @throws ConfigException if the key already refers to a nested
   *     AttributeMap
   */
  def setList(key: String, value: Seq[String]): Unit

  /**
   * Returns true if this map contains the given key.
   */
  def contains(key: String): Boolean

  /**
   * Remove an entry with the given key, if it exists. Returns true if
   * an entry was actually removed, false if not.
   */
  def remove(key: String): Boolean

  /**
   * Return an iterator across the keys of this map.
   */
  def keys: Iterator[String]

  /**
   * Return a new (immutable) map containing a deep copy of all the keys
   * and values from this AttributeMap. Keys from nested maps will be
   * compound (like <code>"inner.name"</code>).
   */
  def asMap(): Map[String, String]

  /**
   * Subscribe to changes on this map. Any changes (including deletions)
   * that occur on this node will be sent through the subscriber to
   * validate and possibly commit. See {@link Subscriber} for details
   * on the validate/commit process.
   *
   * @return a key which can be used to cancel the subscription
   */
  def subscribe(subscriber: Subscriber): SubscriptionKey


  // -----  convenience methods

  /**
   * If the requested key is present, return its value. Otherwise, return
   * the given default value.
   */
  def getString(key: String, defaultValue: String): String = {
    getString(key) match {
      case Some(x) => x
      case None => defaultValue
    }
  }

  /**
   * If the requested key is present and can be converted into an int
   * (via <code>Integer.parseInt</code>), return that int. Otherwise,
   * return <code>None</code>.
   */
  def getInt(key: String): Option[Int] = {
    getString(key) match {
      case Some(x) => {
        try {
          Some(Integer.parseInt(x))
        } catch {
          case _ => None
        }
      }
      case None => None
    }
  }

  /**
   * If the requested key is present and can be converted into an int
   * (via <code>Integer.parseInt</code>), return that int. Otherwise,
   * return the given default value.
   */
  def getInt(key: String, defaultValue: Int): Int = {
    getInt(key) match {
      case Some(n) => n
      case None => defaultValue
    }
  }

  /**
   * If the requested key is present and can be converted into a bool
   * (by being either <code>"true"</code> or <code>"false"</code>),
   * return that bool. Otherwise, return <code>None</code>.
   */
  def getBool(key: String): Option[Boolean] = {
    getString(key) match {
      case Some(x) => Some(x.equals(TRUE))
      case None => None
    }
  }

  /**
   * If the requested key is present and can be converted into a bool
   * (by being either <code>"true"</code> or <code>"false"</code>),
   * return that bool. Otherwise, return the given default value.
   */
  def getBool(key: String, defaultValue: Boolean): Boolean = {
    getBool(key) match {
      case Some(b) => b
      case None => defaultValue
    }
  }

  /**
   * Set the given key to an int value, by converting it to a string
   * first.
   */
  def setInt(key: String, value: Int): Unit = setString(key, value.toString)

  /**
   * Set the given key to a bool value, by converting it to a string
   * first.
   */
  def setBool(key: String, value: Boolean): Unit = {
    setString(key, if (value) TRUE else FALSE)
  }

  /**
   * Return the keys of this map, in sorted order.
   */
  def sortedKeys() = {
    // :( why does this have to be done manually?
    val keys = this.keys.toList.toArray
    Sorting.quickSort(keys)
    keys
  }

  /**
   * Subscribe to changes on this AttributeMap, but don't bother with
   * validating. Whenever this AttributeMap changes, a new copy will be
   * passed to the given function.
   */
  def subscribe(f: (Option[ConfigMap]) => Unit): SubscriptionKey = {
    subscribe(new Subscriber {
      def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
      def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = {
        f(replacement)
      }
    })
  }


  /**
   * If the requested key is present, return its value as a string. Otherwise, throw a
   * ConfigException. <code>toInt</code> and <code>toBoolean</code> may be called on the
   * returned string if an int or bool is desired.
   */
  def apply(key: String): String = getString(key) match {
    case None => throw new ConfigException("undefined config: " + key)
    case Some(v) => v
  }

  /** Equivalent to <code>getString(key, defaultValue)</code>. */
  def apply(key: String, defaultValue: String) = getString(key, defaultValue)

  /** Equivalent to <code>getInt(key, defaultValue)</code>. */
  def apply(key: String, defaultValue: Int) = getInt(key, defaultValue)

  /** Equivalent to <code>getBool(key, defaultValue)</code>. */
  def apply(key: String, defaultValue: Boolean) = getBool(key, defaultValue)

  /** Equivalent to <code>setString(key, value)</code>. */
  def update(key: String, value: String) = setString(key, value)

  /** Equivalent to <code>setInt(key, value)<code>. */
  def update(key: String, value: Int) = setInt(key, value)

  /** Equivalent to <code>setBool(key, value)<code>. */
  def update(key: String, value: Boolean) = setBool(key, value)

  /** Equivalent to <code>setList(key, value)<code>. */
  def update(key: String, value: Seq[String]) = setList(key, value)
}
