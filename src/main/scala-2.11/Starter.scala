import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

/**
 * Created by Jack Daniels on 16.05.2015.
 */
class Starter {
  val config = ConfigFactory.load()
  implicit val system = ActorSystem("Fast_Space")
  val host = config.getString("app.host")
  val port = config.getInt("app.port")


  def start() = {
    if (!system.isTerminated) {
      system.actorOf(Server.serverProps(host, port), "Server")
    }
  }

  def stop() = {
    system.shutdown()
  }
}


object Starter extends App {
  val starter = new Starter()
  starter.start()
}
