package net.lag.configgy

import java.util.regex.Pattern
import scala.collection.{immutable, mutable, Map}
import net.lag.extensions._


private[configgy] abstract class Cell
private[configgy] case class StringCell(value: String) extends Cell
private[configgy] case class AttributesCell(attr: Attributes) extends Cell
private[configgy] case class StringListCell(array: Array[String]) extends Cell


/**
 * Actual implementation of ConfigMap.
 * Stores items in Cell objects, and handles interpolation and key recursion.
 */
private[configgy] class Attributes(val config: Config, val name: String) extends ConfigMap {

  private val cells = new mutable.HashMap[String, Cell]
  private var monitored = false
  private var inherit: Option[Attributes] = None


  def keys: Iterator[String] = cells.keys

  override def toString() = {
    val buffer = new StringBuilder("{")
    buffer ++= name
    if (inherit.isDefined) {
      buffer ++= " (inherit="
      buffer ++= inherit.get.name
      buffer ++= ")"
    }
    buffer ++= ": "
    for (val key <- sortedKeys) {
      buffer ++= key
      buffer ++= "="
      buffer ++= (cells(key) match {
        case StringCell(x) => "\"" + x.quoteC + "\""
        case AttributesCell(x) => x.toString
        case StringListCell(x) => x.mkString("[", ",", "]")
      })
      buffer ++= " "
    }
    buffer ++= "}"
    buffer.toString
  }

  override def equals(obj: Any) = {
    if (! obj.isInstanceOf[Attributes]) {
      false
    } else {
      val other = obj.asInstanceOf[Attributes]
      (other.sortedKeys.toList == sortedKeys.toList) &&
        (cells.keys forall (k => { cells(k) == other.cells(k) }))
    }
  }

  /**
   * Look up a value cell for a given key. If the key is compound (ie,
   * "abc.xyz"), look up the first segment, and if it refers to an inner
   * Attributes object, recursively look up that cell. If it's not an
   * Attributes or it doesn't exist, return None. For a non-compound key,
   * return the cell if it exists, or None if it doesn't.
   */
  private def lookupCell(key: String): Option[Cell] = {
    val elems = key.split("\\.", 2)
    if (elems.length > 1) {
      cells.get(elems(0).toLowerCase) match {
        case Some(AttributesCell(x)) => x.lookupCell(elems(1).toLowerCase)
        case None => inherit match {
          case Some(a) => a.lookupCell(key)
          case None => None
        }
        case _ => None
      }
    } else {
      cells.get(elems(0).toLowerCase) match {
        case x @ Some(_) => x
        case None => inherit match {
          case Some(a) => a.lookupCell(key)
          case None => None
        }
      }
    }
  }

  /**
   * Determine if a key is compound (and requires recursion), and if so,
   * return the nested Attributes block and simple key that can be used to
   * make a recursive call. If the key is simple, return None.
   *
   * <p> If the key is compound, but nested Attributes objects don't exist
   * that match the key, an attempt will be made to create the nested
   * Attributes objects. If one of the key segments already refers to an
   * attribute that isn't a nested Attribute object, a ConfigException
   * will be thrown.
   *
   * <p> For example, for the key "a.b.c", the Attributes object for "a.b"
   * and the key "c" will be returned, creating the "a.b" Attributes object
   * if necessary. If "a" or "a.b" exists but isn't a nested Attributes
   * object, then an ConfigException will be thrown.
   */
  @throws(classOf[ConfigException])
  private def recurse(key: String): Option[(Attributes, String)] = {
    val elems = key.split("\\.", 2)
    if (elems.length > 1) {
      val attr = (cells.get(elems(0).toLowerCase) match {
        case Some(AttributesCell(x)) => x
        case Some(_) => throw new ConfigException("Illegal key " + key)
        case None => createNested(elems(0))
      })
      attr.recurse(elems(1)) match {
        case ret @ Some((a, b)) => ret
        case None => Some((attr, elems(1)))
      }
    } else {
      None
    }
  }

  private def createNested(key: String): Attributes = {
    val attr = new Attributes(config, if (name.equals("")) key else (name + "." + key))
    if (monitored) {
      attr.setMonitored
    }
    cells += Pair(key, new AttributesCell(attr))
    attr
  }

  def getString(key: String): Option[String] = {
    lookupCell(key) match {
      case Some(StringCell(x)) => Some(x)
      case Some(StringListCell(x)) => Some(x.toList.mkString("[", ",", "]"))
      case _ => None
    }
  }

  def getConfigMap(key: String): Option[ConfigMap] = {
    lookupCell(key) match {
      case Some(AttributesCell(x)) => Some(x)
      case _ => None
    }
  }

  private[configgy] def makeAttributes(key: String): Attributes = {
    if (key == "") {
      return this
    }
    recurse(key) match {
      case Some((attr, name)) => attr.makeAttributes(name)
      case None => lookupCell(key) match {
        case Some(AttributesCell(x)) => x
        case Some(_) => throw new ConfigException("Illegal key " + key)
        case None => createNested(key)
      }
    }
  }

  def getList(key: String): Seq[String] = {
    lookupCell(key) match {
      case Some(StringListCell(x)) => x
      case Some(StringCell(x)) => Array[String](x)
      case _ => Array[String]()
    }
  }

  def setString(key: String, value: String): Unit = {
    if (monitored) {
      config.deepSet(name, key, value)
      return
    }

    recurse(key) match {
      case Some((attr, name)) => attr.setString(name, value)
      case None => cells.get(key.toLowerCase) match {
        case Some(AttributesCell(x)) => throw new ConfigException("Illegal key " + key)
        case _ => cells.put(key.toLowerCase, new StringCell(value))
      }
    }
  }

  def setList(key: String, value: Seq[String]): Unit = {
    if (monitored) {
      config.deepSet(name, key, value)
      return
    }

    recurse(key) match {
      case Some((attr, name)) => attr.setList(name, value)
      case None => cells.get(key.toLowerCase) match {
        case Some(AttributesCell(x)) => throw new ConfigException("Illegal key " + key)
        case _ => cells.put(key, new StringListCell(value.toArray))
      }
    }
  }

  def contains(key: String): Boolean = {
    recurse(key) match {
      case Some((attr, name)) => attr.contains(name)
      case None => cells.contains(key.toLowerCase)
    }
  }

  def remove(key: String): Boolean = {
    if (monitored) {
      return config.deepRemove(name, key)
    }

    recurse(key) match {
      case Some((attr, name)) => attr.remove(name)
      case None => {
        cells.removeKey(key.toLowerCase) match {
          case Some(_) => true
          case None => false
        }
      }
    }
  }

  def asMap: Map[String, String] = {
    var ret = immutable.Map.empty[String, String]
    for (val (key, value) <- cells) {
      value match {
        case StringCell(x) => ret = ret.update(key, x)
        case StringListCell(x) => ret = ret.update(key, x.mkString("[", ",", "]"))
        case AttributesCell(x) =>
          for (val (k, v) <- x.asMap) {
            ret = ret.update(key + "." + k, v)
          }
      }
    }
    ret
  }

  def subscribe(subscriber: Subscriber) = {
    config.subscribe(name, subscriber)
  }

  // substitute "$(...)" strings with looked-up vars
  // (and find "\$" and replace them with "$")
  private val INTERPOLATE_RE = """(?<!\\)\$\((\w[\w\d\._-]*)\)|\\\$""".r

  protected[configgy] def interpolate(s: String): String = {
    def lookup(key: String, path: List[ConfigMap]): String = {
      path match {
        case Nil => ""
        case attr :: xs => attr.getString(key) match {
          case Some(x) => x
          case None => lookup(key, xs)
        }
      }
    }

    s.regexSub(INTERPOLATE_RE) { m =>
      if (m.matched == "\\$") {
        "$"
      } else if (config == null) {
        lookup(m.group(0), List(this, EnvironmentAttributes))
      } else {
        lookup(m.group(0), List(this, config, EnvironmentAttributes))
      }
    }
  }

  protected[configgy] def interpolate(key: String, s: String): String = {
    recurse(key) match {
      case Some((attr, name)) => attr.interpolate(s)
      case None => interpolate(s)
    }
  }

  /* set this node as part of a monitored config tree. once this is set,
   * all modification requests go through the root Config, so validation
   * will happen.
   */
  protected[configgy] def setMonitored: Unit = {
    if (monitored) {
      return
    }

    monitored = true
    for (val cell <- cells.values) {
      cell match {
        case AttributesCell(x) => x.setMonitored
        case _ => // pass
      }
    }
  }

  protected[configgy] def isMonitored = monitored

  protected[configgy] def inheritFrom(attr: Attributes) = {
    inherit = Some(attr)
  }

  // make a deep copy of the Attributes tree.
  def copy: Attributes = {
    val out = new Attributes(config, name)
    for (val (key, value) <- cells.elements) {
      value match {
        case StringCell(x) => out(key) = x
        case StringListCell(x) => out(key) = x
        case AttributesCell(x) =>
          val attr = x.copy
          out.cells += Pair(key, new AttributesCell(attr))
      }
    }
    out
  }
}
