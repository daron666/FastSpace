import akka.actor.{Props, ActorRef, Actor}
import akka.event.Logging

class Counter(game: ActorRef) extends Actor{
  import CounterProtocol._

  val log = Logging(context.system, this)

  def receive = {
    case Count(message) =>
      log.info(message)
      Thread.sleep(1000)
      game ! Broadcast(1)
      Thread.sleep(1000)
      game ! Broadcast(2)
      Thread.sleep(1000)
      game ! Broadcast(3)
      log.info(s"Stopping counter #${self.path.toStringWithoutAddress}")
      context stop self
  }

}

object Counter {
  def counterProps(game: ActorRef) = Props(new Counter(game))
}

object CounterProtocol {
  case class Count(message: String)
  case class Broadcast(tick: Int)
}
