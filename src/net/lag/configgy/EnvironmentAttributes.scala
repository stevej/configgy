package net.lag.configgy

import java.net.InetAddress

import scala.collection.jcl
import scala.collection.mutable

// grr, scala can wrap any specific java Map type, but not the generic Map. why not?
class JavaMap[K, E](override val underlying: java.util.Map[K, E]) extends jcl.MapWrapper[K, E]


// an AttributeMap that wraps the system environment
object EnvironmentAttributes extends AttributeMap {
    private val env = new mutable.HashMap[String, String]
    env ++= new JavaMap(System.getenv()).elements
    
    def get(key: String): Option[String] = env.get(key)
    
    def getAttributes(key: String): Option[AttributeMap] = None
        
    def getStringList(key: String): Option[Array[String]] = get(key) match {
        case None => None
        case Some(x) => Some(Array[String](x))
    }
    
    def set(key: String, value: String): Unit = error("read-only attributes")
    def set(key: String, value: Array[String]): Unit = error("read-only attributes")
    def contains(key: String): Boolean = env.contains(key)
    def remove(key: String): Boolean = error("read-only attributes")
    def keys: Iterator[String] = env.keys
    def asMap: Map[String, String] = error("not implemented")
    def subscribe(subscriber: Subscriber): SubscriptionKey = error("not implemented")
    
    
    try {
        val addr = InetAddress.getLocalHost
        val ip = addr.getHostAddress
        val dns = addr.getHostName
        
        if (ip != null) {
            env("HOSTIP") = ip
        }
        if (dns != null) {
            env("HOSTNAME") = dns
        }
    } catch {
        case _ => // pass
    }
}
