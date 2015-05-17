import akka.actor.{Props, ActorRef, Actor}
import akka.event.Logging

import scala.util.Random

class Counter(game: ActorRef) extends Actor{
  import CounterProtocol._

  val log = Logging(context.system, this)
  val random = new Random()

  def receive = {
    case Count(message) =>
      log.info(message)
      Thread.sleep(nextDuration())
      game ! Broadcast(1)
      Thread.sleep(nextDuration())
      game ! Broadcast(2)
      Thread.sleep(nextDuration())
      game ! Broadcast(3)
      log.info(s"Stopping counter #${self.path.toStringWithoutAddress}")
      context stop self
  }

  private def nextDuration(): Long = {
    ((2 + random.nextDouble() * 2) * 1000).toLong
  }
}

object Counter {
  def counterProps(game: ActorRef) = Props(new Counter(game))
}

object CounterProtocol {
  case class Count(message: String)
  case class Broadcast(tick: Int)
}
