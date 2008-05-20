package net.lag.configgy

import scala.collection.Map
import scala.util.Sorting


/**
 * Abstract trait for an object that maps string keys to values of type
 * string, string array, or (nested) AttributeMap. Integers and booleans may
 * also be stored and retrieved, but they are converted to/from strings in
 * the process.
 */
trait AttributeMap {
    private val TRUE = "true"
    private val FALSE = "false"


    // -----  required methods
    
    def get(key: String): Option[String]
    def getAttributes(key: String): Option[AttributeMap]
    def getStringList(key: String): Option[Array[String]]
    def set(key: String, value: String): Unit
    def set(key: String, value: Array[String]): Unit
    def contains(key: String): Boolean
    def remove(key: String): Boolean
    def keys: Iterator[String]
    def asMap: Map[String, String]
    def subscribe(subscriber: Subscriber): SubscriptionKey

    
    // -----  convenience methods
    
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
    
    def set(key: String, value: Int): Unit = set(key, value.toString)
    
    def set(key: String, value: Boolean): Unit = {
        if (value) {
            set(key, TRUE)
        } else {
            set(key, FALSE)
        }
    }
    
    // why does this have to be done manually?
    def sortedKeys = {
        val keys = this.keys.toList.toArray
        Sorting.quickSort(keys)
        keys
    }
    
    /**
     * Subscribe to changes on this AttributeMap, but don't bother with
     * validating. Whenever this AttributeMap changes, a new copy will be
     * passed to the given function.
     */
    def subscribe(f: (Option[AttributeMap]) => Unit): SubscriptionKey = {
        subscribe(new Subscriber {
            def validate(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit = { }
            def commit(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit = {
                f(replacement)
            }
        })
    }
                    
    
    def apply(key: String): Option[String] = get(key)
    def apply(key: String, defaultValue: String) = get(key, defaultValue)
    def update(key: String, value: String) = set(key, value)
    def update(key: String, value: Int) = set(key, value)
    def update(key: String, value: Boolean) = set(key, value)
    def update(key: String, value: Array[String]) = set(key, value)
}
