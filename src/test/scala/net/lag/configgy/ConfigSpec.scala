package net.lag.configgy

import java.io.{File, FileOutputStream}
import net.lag.TestHelper

import org.specs._


object ConfigSpec extends Specification with TestHelper {

  class FakeSubscriber extends Subscriber {
    def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
    def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
  }

  // remembers the before & after config nodes when committing a change
  class MemorySubscriber extends Subscriber {
    var used = false
    var savedCurrent: Option[ConfigMap] = None
    var savedReplacement: Option[ConfigMap] = None

    def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
    def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = {
      used = true
      savedCurrent = current
      savedReplacement = replacement
    }
  }


  // refuses any change to its node.
  class AngrySubscriber extends Subscriber {
    def validate(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = throw new ValidationException("no way!")
    def commit(current: Option[ConfigMap], replacement: Option[ConfigMap]): Unit = { }
  }


  "Config" should {
    "take subscriptions" in {
      val c = new Config
      var id = c.subscribe("alpha.beta.gamma", new FakeSubscriber)

      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=0 { gamma=1 } } }"
      c.unsubscribe(id)
      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=0 { gamma=0 } } }"
      id = c.subscribe("alpha.beta") { (attr: Option[ConfigMap]) => Console.println("hello") }
      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=1 { gamma=0 } } }"
      c.unsubscribe(id)
      c.debugSubscribers mustEqual "subs=0 { alpha=0 { beta=0 { gamma=0 } } }"
    }

    "call subscribers" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"

      var checked = false
      c.subscribe("alpha.beta") { (attr: Option[ConfigMap]) => checked = true }
      checked mustBe false
      c("alpha.beta.delta") = "goodbye"
      checked mustBe true
    }

    "call subscribers with the old & new data" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"

      val sub = new MemorySubscriber
      c.subscribe("alpha.beta", sub)
      sub.used mustBe false

      c("alpha.beta.delta") = "goodbye"
      sub.used mustBe true
      sub.savedCurrent.get.toString mustEqual "{alpha.beta: gamma=\"hello\" }"
      sub.savedReplacement.get.toString mustEqual "{alpha.beta: delta=\"goodbye\" gamma=\"hello\" }"
      c.toString mustEqual "{: alpha={alpha: beta={alpha.beta: delta=\"goodbye\" gamma=\"hello\" } } }"

      c("alpha.beta.gamma") = "gutentag"
      sub.savedCurrent.get.toString mustEqual "{alpha.beta: delta=\"goodbye\" gamma=\"hello\" }"
      sub.savedReplacement.get.toString mustEqual "{alpha.beta: delta=\"goodbye\" gamma=\"gutentag\" }"
      c.toString mustEqual "{: alpha={alpha: beta={alpha.beta: delta=\"goodbye\" gamma=\"gutentag\" } } }"
    }

    "abort a rejected change" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"

      c.subscribe("alpha.beta", new AngrySubscriber)
      (c("alpha.beta.gamma") = "gutentag") must throwA(new ValidationException(""))
      c("alpha.giraffe") = "tall!"
      c.toString mustEqual "{: alpha={alpha: beta={alpha.beta: gamma=\"hello\" } giraffe=\"tall!\" } }"
    }

    "deal correctly with multiple subscribers at different nodes" in {
      val c = new Config
      c("alpha.beta.gamma") = "hello"
      c("alpha.giraffe") = "tall!"
      c("forest.fires.are") = "bad"

      val rootsub = new MemorySubscriber
      c.subscribe(rootsub)
      val firesub = new AngrySubscriber
      c.subscribe("forest.fires", firesub)
      val betasub = new MemorySubscriber
      c.subscribe("alpha.beta", betasub)

      c("unrelated") = 39
      rootsub.used mustBe true
      betasub.used mustBe false
      rootsub.savedCurrent.get.toString mustEqual
        "{: alpha={alpha: beta={alpha.beta: gamma=\"hello\" } giraffe=\"tall!\" } forest={forest: fires={forest.fires: are=\"bad\" } } }"
      rootsub.savedReplacement.get.toString mustEqual
        "{: alpha={alpha: beta={alpha.beta: gamma=\"hello\" } giraffe=\"tall!\" } forest={forest: fires={forest.fires: are=\"bad\" } } unrelated=\"39\" }"

      rootsub.used = false
      c("forest.matches") = false
      rootsub.used mustBe true
      betasub.used mustBe false
      c.getConfigMap("forest").get.toString mustEqual
        "{forest: fires={forest.fires: are=\"bad\" } matches=\"false\" }"

      c.remove("forest") must throwA(new ValidationException(""))

      rootsub.used = false
      betasub.used = false
      c("alpha.beta.gamma") = "goodbye"
      rootsub.used mustBe true
      betasub.used mustBe true
      betasub.savedCurrent.get.toString mustEqual "{alpha.beta: gamma=\"hello\" }"
      betasub.savedReplacement.get.toString mustEqual "{alpha.beta: gamma=\"goodbye\" }"
    }

    "include relative files" in {
      withTempFolder {
        val inner = new File(folderName, "inner")
        inner.mkdir

        val data1 = "fruit = 17\ninclude \"inner/punch.conf\"\n"
        val f1 = new FileOutputStream(new File(folderName, "fruit.conf"))
        f1.write(data1.getBytes)
        f1.close
        val data2 = "punch = 23\n"
        val f2 = new FileOutputStream(new File(inner, "punch.conf"))
        f2.write(data2.getBytes)
        f2.close

        val c = new Config
        c.loadFile(folderName, "fruit.conf")
        c.toString mustEqual "{: fruit=\"17\" punch=\"23\" }"
      }
    }

    "include from a resource" in {
      /* kinda cheaty: we know the current folder is the project root,
       * so we can stuff something in build-test/ briefly to get it to
       * appear in the classpath.
       */
      val tempFilename = new File(new File(".").getAbsolutePath, "target/test-classes/happy.conf")
      try {
        val data1 = "commie = 501\n"
        val f1 = new FileOutputStream(tempFilename)
        f1.write(data1.getBytes)
        f1.close

        val c = new Config
        c.importer = new ResourceImporter
        c.load("include \"happy.conf\"\n")
        c.toString mustEqual "{: commie=\"501\" }"
      } finally {
        tempFilename.delete
      }
    }
  }
}
