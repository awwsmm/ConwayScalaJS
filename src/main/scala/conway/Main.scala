package conway

import akka.actor.ActorSystem
import conway.actors.GameActor
import org.scalajs.dom
import org.scalajs.dom.{document, window}
import scalatags.JsDom.all._

import scala.util.Random

object Main {

  implicit val system: ActorSystem = ActorSystem()

  val logging: Boolean = false

  def main(args: Array[String]): Unit = {

    val organismSize = 40
    val padding = 2
    val margin = 100

    val nCols: Int = ((window.innerWidth - 2*margin) / (organismSize + padding)).toInt
    val nRows: Int = ((window.innerHeight - 2*margin) / (organismSize + padding)).toInt

    val gameMap = randomMap(nRows, nCols)
    val state = GameActor.State(gameMap, organismSize, padding, margin, logging)
    val game = system.actorOf(GameActor.props(state, logging))

    // help menu
    val menu = div(
      position := "absolute",
      bottom := 20,
      width := 500,
      left := "50%",
      marginLeft := -250,
      textAlign := "center",

      p("Click (or tap on mobile) on a square to change its color / state."),
      div(
        button("Tick",
          onclick := { () => game ! GameActor.Tick }
        ),
        button("Toggle",
          onclick := { () => game ! GameActor.Toggle }
        ),
      ),
      p("Or, on desktop, press 't' to step, press 's' to toggle auto-stepping on / off.")
    ).render

    document.addEventListener("DOMContentLoaded", (_: dom.Event) => {
      println(s"gameMap is\n$gameMap")
      state.organisms
      document.body.appendChild(menu)
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

