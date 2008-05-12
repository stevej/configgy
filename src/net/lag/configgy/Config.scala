package net.lag.configgy

import java.io.File
import scala.collection.{Map, Set}
import scala.collection.{immutable, mutable}
import net.lag.ConfiggyExtensions._


private abstract class Phase
private case object VALIDATE_PHASE extends Phase
private case object COMMIT_PHASE extends Phase


private class SubscriptionNode {
    var subscribers = new mutable.HashSet[Subscriber]
    var map = new mutable.HashMap[String, SubscriptionNode]
    
    def get(name: String): SubscriptionNode = {
        map.get(name) match {
            case Some(x) => x
            case None => {
                val node = new SubscriptionNode
                map(name) = node
                node
            }
        }
    }
    
    override def toString = {
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
    def validate(key: List[String], current: Option[AttributeMap], replacement: Option[AttributeMap], phase: Phase): Unit = {
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
                    case Some(node) => nextNodes = immutable.HashSet((segment, node)).elements
                }
            }
        }
        
        for (val (segment, node) <- nextNodes) {
            val subCurrent = current match {
                case None => None
                case Some(x) => x.getAttributes(segment)
            }
            val subReplacement = replacement match {
                case None => None
                case Some(x) => x.getAttributes(segment)
            }
            node.validate(if (key == Nil) Nil else key.tail, subCurrent, subReplacement, phase)
        }
    }
}


class Config extends AttributeMap {
    private var root = new Attributes(this, "")
    private val subscribers = new SubscriptionNode
    private val subscriberKeys = new mutable.HashMap[Int, (SubscriptionNode, Subscriber)]
    private var nextKey = 1
    var importer: Importer = new FilesystemImporter(new File(".").getCanonicalPath)

    def load(data: String) = {
        new ConfigParser(root, importer).parse(data)
        root.setMonitored
    }
    
    def loadFile(filename: String) = {
        load(importer.importFile(filename))
    }
    
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
    
    private[configgy] def subscribe(key: String)(f: (Option[AttributeMap]) => Unit): SubscriptionKey = {
        subscribe(key, new Subscriber {
            def validate(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit = { }
            def commit(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit = {
                f(replacement)
            }
        })
    }
    
    def subscribe(subscriber: Subscriber) = subscribe(null.asInstanceOf[String], subscriber)
    
    override def subscribe(f: (Option[AttributeMap]) => Unit) = subscribe(null.asInstanceOf[String])(f)
    
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
    
    def debugSubscribers = synchronized {
        "subs=" + subscribers.toString
    }
    
    
    // -----  modifications that happen within monitored Attributes nodes
    
    @throws(classOf[ValidationException])
    private def deepChange(name: String, key: String, operation: (AttributeMap, String) => Boolean): Boolean = synchronized {
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
    
    private[configgy] def deepSet(name: String, key: String, value: Array[String]) = {
        deepChange(name, key, { (newRoot, fullKey) => newRoot(fullKey) = value; true })
    }
    
    private[configgy] def deepRemove(name: String, key: String): Boolean = {
        deepChange(name, key, { (newRoot, fullKey) => newRoot.remove(fullKey) })
    }
    
    
    // -----  implement AttributeMap by wrapping our root object:
    
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
