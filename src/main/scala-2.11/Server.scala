import java.net.InetSocketAddress

import akka.actor.{Terminated, Actor, ActorRef, Props}
import akka.event.Logging
import akka.io.Tcp._
import akka.io.{IO, Tcp}

/**
 * Created by Jack Daniels on 16.05.2015.
 */
class Server(host: String, port: Int) extends Actor {
  import PlayerProtocol._
  import GameProtocol._
  import Server._
  import context.system

  val log = Logging(system, this)
  var connectionNum: Int = 1
  var gameNum: Int = 1
  var waitingPlayer: Option[ActorRef] = None

  log.info(STARTING_SERVER)

  IO(Tcp) ! Bind(self, new InetSocketAddress(host, port))

  def receive = {
    case b @ Bound(localAddress) =>
      log.info(PORT_BOUND, localAddress)

    case c @ Connected(remote, local) =>
      log.info(CONNECTION_ACCEPTED)
      val player = context.actorOf(Player.playerProps(sender(), connectionNum),s"player_$connectionNum")
      player ! Welcome("Welcome! Trying to find opponent for you.\r\n")
      waitingPlayer match {
        case Some(oldPlayer) => {
          val game = context.actorOf(Game.gameProps(oldPlayer, player, gameNum),s"game_$gameNum")
          gameNum += 1
          game ! Start(s"Game #$gameNum have started")
          context watch game
          waitingPlayer = None
          log.info("Creating game.")
        }
        case None => {
          waitingPlayer = Some(player)
          log.info("Connected player added to waiting queue.")
        }
      }
      connectionNum += 1

    case CommandFailed(_: Bind) =>
      log.error(BINDING_FAILED)
      context stop self

    case Terminated(game) =>
      log.info(s"Game with addres ${game.path.toStringWithoutAddress} have stopped.")
  }
}

object Server {
  val STARTING_SERVER = "Server is starting."
  val PORT_BOUND = "Port is {}"
  val BINDING_FAILED = "Cannot bind port, stopping server."
  val CONNECTION_ACCEPTED = "Incoming connection accepted."
  def serverProps(host: String,port: Int): Props = Props(new Server(host, port))
}