import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import akka.io.Tcp._
import akka.util.ByteString

/**
 * Created by Jack Daniels on 17.05.2015.
 */
class Player(connection: ActorRef, number: Int) extends Actor {
  import GameProtocol._
  import PlayerProtocol._

  val log = Logging(context.system, this)

  connection ! Register(self)

  context.watch(connection)

  var game: Option[ActorRef] = None
  var gameStarted = false
  var isFirstMessage = true
  var winner = false

  def receive = {
    case Welcome(message) => connection ! Write(ByteString(message))
    case GameStart(message) => {
      game = Some(sender())
      gameStarted = true
      connection ! Write(ByteString(message))
    }
    case Received(data) => {
      if (isFirstMessage) {
        isFirstMessage = false
      } else if (gameStarted) {
        game match {
          case Some(gameActorRef) => {
            log.info(s"Data from player ${data.utf8String.replaceAll("[\\r\\n]", "")}")
            gameActorRef ! InputData(data.utf8String.replaceAll("[\\r\\n]", ""))
          }
          case None => Unit
        }
      }
    }
    case PeerClosed     => {
      log.info(s"Player #$number have disconnected")
      context stop self
    }
    case Show(message) => connection ! Write(ByteString(message))
    case Won(message) => {
      connection ! Write(ByteString(message))
      winner = true
      context stop self
    }
    case Lost(message) =>  {
      connection ! Write(ByteString(message))
      context stop self
    }
  }

  override def postStop() = {
    log.info(s"Player #$number have stopped by game")

  }
}

object Player {
  def playerProps(connection: ActorRef, number: Int): Props = Props(new Player(connection, number))
}

object PlayerProtocol {
  case class Welcome(messgae: String)
  case class GameStart(message: String)
  case class Show(message: String)
  case class InputData(message: String)
}
