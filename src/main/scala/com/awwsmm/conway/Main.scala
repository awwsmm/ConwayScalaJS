package com.awwsmm.conway

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.awwsmm.conway.actors.GameActor
import org.scalajs.dom
import org.scalajs.dom.{document, window}
import scalatags.JsDom.all._

import scala.util.Random

object Main {

  implicit val typedSystem: ActorSystem[Nothing] = ActorSystem[Nothing](Behaviors.ignore, "typedSystem")

  val logging: Boolean = false

  def main(args: Array[String]): Unit = {

    val organismSize = 40
    val padding = 2
    val margin = 100

    val nCols: Int = ((window.innerWidth - 2*margin) / (organismSize + padding)).toInt
    val nRows: Int = ((window.innerHeight - 2*margin) / (organismSize + padding)).toInt

    val gameMap = randomMap(nRows, nCols)
    val typedState = GameActor.Landscape(gameMap)
    val typedConfig = GameActor.Config(organismSize, padding, margin)
    val typedGame = typedSystem.systemActorOf(GameActor(typedConfig, typedState), "gameActor")

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
            onclick := { () => typedGame ! GameActor.Tick }
        ),
        button("Toggle",
            onclick := { () => typedGame ! GameActor.Toggle }
        ),
      ),
      p("Or, on desktop, press 't' to step, press 's' to toggle auto-stepping on / off.")
    ).render

    document.addEventListener("DOMContentLoaded", (_: dom.Event) => {
      println(s"gameMap is\n$gameMap")
      document.body.appendChild(menu)
    })

    document.addEventListener("keydown", (k: dom.KeyboardEvent) => {
      if (k.key == "t") typedGame ! GameActor.Tick
      if (k.key == "s") typedGame ! GameActor.Toggle
    })
  }

  def randomMap (rows: Int, cols: Int): String = {
    def randomRow(): String = {
      val bools = (1 to cols).map(_ => Random.nextBoolean())
      val chars = bools.map(if (_) "_" else "X")
      chars mkString ""
    }

    (1 to rows).map(_ => randomRow()) mkString "\n"
  }

}

