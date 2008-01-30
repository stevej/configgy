package net.lag.configgy

import scala.collection.Map


class Config extends AttributeMap {
    private val root = new Attributes(this, "")
    
    
    // implement AttributeMap by wrapping our root object:
    
    def get(key: String): Option[String] = root.get(key)
    def getAttributes(key: String): Option[AttributeMap] = root.getAttributes(key)
    def getStringList(key: String): Option[Array[String]] = root.getStringList(key)
    def set(key: String, value: String): Unit = root.set(key, value)
    def set(key: String, value: Array[String]): Unit = root.set(key, value)
    def contains(key: String): Boolean = root.contains(key)
    def remove(key: String): Boolean = root.remove(key)
    def keys: Iterator[String] = root.keys
    def asMap: Map[String, String] = root.asMap
}
