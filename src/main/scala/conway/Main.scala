package conway

import akka.actor.ActorSystem
import conway.actors.GameActor
import org.scalajs.dom
import org.scalajs.dom.document

object Main {

  implicit val system: ActorSystem = ActorSystem()

  def main(args: Array[String]): Unit = {

    val gameMap1 =
      GameActor.GameMap(
        """_____
          |__X__
          |__X__
          |__X__
          |_____"""
      )

    def randomMap (rows: Int, cols: Int) = GameActor.GameMap(
      (for {
        row <- 1 to rows
      } yield {
        (for {
          col <- 1 to cols
          rand = scala.util.Random.nextBoolean()
        } yield if (rand) "X" else "_").mkString("")
      }).mkString("\n")
    )

    val gameMap = randomMap(20, 20)

    val state = GameActor.State(gameMap, 20, 5, 50)
    val game = system.actorOf(GameActor.props(state))

    document.addEventListener("DOMContentLoaded", (_: dom.Event) => {
      println(s"gameMap is\n$gameMap")
      state.organisms
    })

    document.addEventListener("keydown", (k: dom.KeyboardEvent) => {
      if (k.key == "t") game ! GameActor.Tick
    })
  }

}

