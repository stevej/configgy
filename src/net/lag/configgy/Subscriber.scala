package net.lag.configgy;

trait Subscriber {
    @throws(classOf[ValidationException])
    def validate(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit
    
    def commit(current: Option[AttributeMap], replacement: Option[AttributeMap]): Unit
}

class SubscriptionKey protected[configgy](val config: Config, protected[configgy] val id: Int) {
    def unsubscribe = config.unsubscribe(this)
}

class ValidationException(reason: String) extends Exception(reason)
