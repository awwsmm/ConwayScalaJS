package conway

import akka.actor.ActorSystem
import conway.actors.GameActor
import org.scalajs.dom
import org.scalajs.dom.document

import scala.util.Random

object Main {

  implicit val system: ActorSystem = ActorSystem()

  val logging: Boolean = false

  def main(args: Array[String]): Unit = {

    val gameMap = randomMap(25, 50)

    val state = GameActor.State(gameMap, 20, 5, 50, logging)
    val game = system.actorOf(GameActor.props(state, logging))

    document.addEventListener("DOMContentLoaded", (_: dom.Event) => {
      println(s"gameMap is\n$gameMap")
      state.organisms
    })

    document.addEventListener("keydown", (k: dom.KeyboardEvent) => {
      if (k.key == "t") game ! GameActor.Tick
      if (k.key == "s") game ! GameActor.Toggle
    })
  }

  def randomMap (rows: Int, cols: Int): GameActor.GameMap = {
    def randomRow(): String = {
      val bools = (1 to cols).map(_ => Random.nextBoolean())
      val chars = bools.map(if (_) "_" else "X")
      chars mkString ""
    }

    GameActor.GameMap((1 to rows).map(_ => randomRow()) mkString "\n")
  }

}

