import akka.actor.ActorSystem
import akka.testkit.{DefaultTimeout, ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.concurrent.duration._

class CounterSpecs extends TestKit(ActorSystem("Counter_Tests")) with WordSpecLike with MustMatchers with StopSystem with DefaultTimeout with ImplicitSender {
  import CounterProtocol._

  val counter = system.actorOf(Counter.counterProps(testActor))

  "A Counter must" should {
    "start when recived a start message and count." in {
      var messages = Seq[Int]()
      within(6 seconds,14 seconds) {
        receiveWhile(idle = 5 seconds, messages = 3) {
          case Broadcast(v) => messages = v +: messages
        }
      }
      messages.length must be(3)
      messages.reverse must be(Seq(1,2,3))
    }
  }

}
