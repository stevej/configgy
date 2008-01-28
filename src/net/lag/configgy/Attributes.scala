package net.lag.configgy

import scala.collection.mutable.{HashMap, Map}
import scala.util.Sorting


class AttributesException(reason: String) extends Exception(reason)


class Attributes protected[configgy](val root: Config, val name: String) {
    protected[configgy] class Cell
    private case class StringCell(value: String) extends Cell
    private case class AttributesCell(attr: Attributes) extends Cell
    private case class StringListCell(array: Array[String]) extends Cell
    
    private val TRUE = "true"
    private val FALSE = "false"

    private val cells = new HashMap[String, Cell]
    
    
    // why does this have to be done manually?
    def sortedKeys = {
        val keys = cells.keySet.toArray
        Sorting.quickSort(keys)
        keys
    }
    
    override def toString() = {
        val buffer = new StringBuilder("{")
        buffer ++= name
        buffer ++= ": "
        for (val key <- sortedKeys) {
            buffer ++= key
            buffer ++= "="
            buffer ++= (cells(key) match {
                case StringCell(x) => "\"" + StringUtils.quoteC(x) + "\""
                case AttributesCell(x) => x.toString
                case StringListCell(x) => x.mkString("[", ",", "]")
            })
            buffer ++= " "
        }
        buffer ++= "}"
        buffer.toString
    }
    
    // FIXME: equals
    // unset
    // getkeys

    /**
     * Look up a value cell for a given key. If the key is compound (ie,
     * "abc.xyz"), look up the first segment, and if it refers to an inner
     * Attributes object, recursively look up that cell. If it's not an
     * Attributes or it doesn't exist, return None. For a non-compound key,
     * return the cell if it exists, or None if it doesn't.
     */
    protected[configgy] def lookupCell(key: String): Option[Cell] = {
        val elems = key.split("\\.", 2)
        if (elems.length > 1) {
            cells.get(elems(0)) match {
                // FIXME: i think this cast is exposing a compiler bug?
                case Some(AttributesCell(x)) => x.lookupCell(elems(1)).asInstanceOf[Option[Cell]]
                case _ => None
            }
        } else {
            cells.get(elems(0))
        }
    }
    
    /**
     * Check if a key is compound, and if it is, and refers to a valid inner
     * Attributes object, return that Attributes object and the inner key.
     * Otherwise return None. If the key is compound, but refers to an inner
     * Attributes object that can't exist (because the name is already used 
     * for a string or other non-Attributes value), an AttributesException is 
     * thrown.
     */
    @throws(classOf[AttributesException])
    protected[configgy] def recurse(key: String): Option[(Attributes, String)] = {
        val elems = key.split("\\.", 2)
        if (elems.length > 1) {
            cells.get(elems(0)) match {
                case Some(AttributesCell(x)) => Some((x, elems(1)))
                case Some(_) => throw new AttributesException("Illegal key " + key)
                case None => Some((createNested(elems(0)), elems(1)))
            }
        } else {
            None
        }
    }
    
    private def createNested(key: String): Attributes = {
        val attr = new Attributes(root, if (name.equals("")) key else (name + "." + key))
        cells += key -> new AttributesCell(attr)
        attr
    }
    
    def get(key: String): Option[String] = {
        lookupCell(key) match {
            case Some(StringCell(x)) => Some(x)
            case Some(StringListCell(x)) => Some(x.toList.mkString("[", ",", "]"))
            case _ => None
        }
    }
    
    def get(key: String, defaultValue: String): String = {
        get(key) match {
            case Some(x) => x
            case None => defaultValue
        }
    }
    
    def getInt(key: String): Option[Int] = {
        get(key) match {
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
    
    def getInt(key: String, defaultValue: Int): Int = {
        getInt(key) match {
            case Some(n) => n
            case None => defaultValue
        }
    }
    
    def getBool(key: String): Option[Boolean] = {
        get(key) match {
            case Some(x) => Some(x.equals(TRUE))
            case None => None
        }
    }
    
    def getBool(key: String, defaultValue: Boolean): Boolean = {
        getBool(key) match {
            case Some(b) => b
            case None => defaultValue
        }
    }
    
    def getAttributes(key: String): Option[Attributes] = {
        lookupCell(key) match {
            case Some(AttributesCell(x)) => Some(x)
            case _ => None
        }
    }
    
    def getStringList(key: String): Option[Array[String]] = {
        lookupCell(key) match {
            case Some(StringListCell(x)) => Some(x)
            case Some(StringCell(x)) => Some(Array[String](x))
            case _ => None
        }
    }
    
    def apply(key: String): String = get(key, null)
    
    def set(key: String, value: String): Unit = {
        recurse(key) match {
            case Some((attr, name)) => attr.set(name, value)
            case None => cells.get(key) match {
                case Some(AttributesCell(x)) => throw new AttributesException("Illegal key " + key) 
                case _ => cells += key -> new StringCell(value)
            }
        }
    }
    
    def set(key: String, value: Int): Unit = set(key, value.toString)
    
    def set(key: String, value: Boolean): Unit = {
        if (value) {
            set(key, TRUE)
        } else {
            set(key, FALSE)
        }
    }
    
    def set(key: String, value: Array[String]): Unit = {
        recurse(key) match {
            case Some((attr, name)) => attr.set(name, value)
            case None => cells.get(key) match {
                case Some(AttributesCell(x)) => throw new AttributesException("Illegal key " + key)
                case _ => cells += key -> new StringListCell(value)
            }
        }
    }

    def update(key: String, value: String) = set(key, value)
    def update(key: String, value: Int) = set(key, value)
    def update(key: String, value: Array[String]) = set(key, value)
}
