import akka.actor.{Props, ActorRef, Actor}
import akka.event.Logging

import scala.util.Random
import scala.concurrent.duration._


class Counter(game: ActorRef) extends Actor {

  import CounterProtocol._
  implicit val executionContext = context.system.dispatcher

  val log = Logging(context.system, this)
  val random = new Random()

  var counter = 1

  def receive = {
    case "count" =>
      if (counter <= 3) {
        game ! Broadcast(counter)
        counter += 1
        context.system.scheduler.scheduleOnce(nextDuration() millis, self, "count")
      } else {
        context stop self
      }


  }

  private def nextDuration(): Long = {
    ((2 + random.nextDouble() * 2) * 1000).toLong
  }

  override def preStart() = context.system.scheduler.scheduleOnce(nextDuration() millis, self, "count")

  override def postRestart(reason: Throwable) = {}
}

object Counter {
  def counterProps(game: ActorRef) = Props(new Counter(game))
}

object CounterProtocol {
  case class Count(message: String)
  case class Broadcast(tick: Int)
}