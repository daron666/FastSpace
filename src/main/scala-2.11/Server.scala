import java.net.InetSocketAddress

import akka.actor.{Terminated, Actor, ActorRef, Props}
import akka.event.Logging
import akka.io.Tcp._
import akka.io.{IO, Tcp}

class Server(host: String, port: Int) extends Actor {
  import PlayerProtocol._
  import GameProtocol._
  import Server._
  import context.system

  val log = Logging(system, this)
  var connectionNum: Int = 1
  var gameNum: Int = 1
  var waitingPlayer: Option[ActorRef] = None

  log.info(starting)
  IO(Tcp) ! Bind(self, new InetSocketAddress(host, port))

  def receive = {
    case b@Bound(localAddress) =>
      log.info(bound, localAddress)

    case c@Connected(remote, local) =>
      log.info(connectionAccepted)
      val player = context.actorOf(Player.playerProps(sender(), connectionNum), s"player_$connectionNum")
      player ! Welcome(welcomeMessage)
      waitingPlayer match {
        case Some(oldPlayer) =>
          log.info(creatingGame)
          val game = context.actorOf(Game.gameProps(oldPlayer, player, gameNum), s"game_$gameNum")
          gameNum += 1
          game ! Start(s"Game #$gameNum have started")
          context watch game
          waitingPlayer = None
        case None =>
          waitingPlayer = Some(player)
          log.info(playerAddedToQeueu)
      }
      connectionNum += 1

    case CommandFailed(_: Bind) =>
      log.error(bindingFailsed)
      context stop self

    case Terminated(game) =>
      log.info(gameStopped, game.path.toStringWithoutAddress)
  }
}

object Server {
  val starting = "Server is starting."
  val bound = "Port is {}"
  val bindingFailsed = "Cannot bind port, stopping server."
  val connectionAccepted = "Incoming connection accepted."
  val gameStopped = "Game with addres {} have stopped."
  val playerAddedToQeueu = "Connected player added to waiting queue."
  val creatingGame = "Creating game."
  val welcomeMessage = "Welcome! Trying to find opponent for you.\r\n"
  val lostMessage = "Sorry! You Have Just Lost.\r\n"
  val gameStartMessage = "We have found your opponent. Send me a space when you will see 3! \r\n"
  val playerDisconnected = "Game has watched termination of player {}"
  val winAfterDisconnect = "You Have Just Won Cause Your Opponent Have Disconnected!\r\n"
  val winMessage = "You Have Just Won!\r\n"
  val winAfterFaultOpponent = "You've WON! You opponent have fault started!\r\n"
  val gameFinishedMessage = "Game just have finished! Thanks for participating.\r\n"
  val playerStoppedByGameMessage = "Player #{} have stopped by game"

  def serverProps(host: String,port: Int): Props = Props(new Server(host, port))
}