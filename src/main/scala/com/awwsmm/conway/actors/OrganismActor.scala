package com.awwsmm.conway.actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import org.scalajs.dom.document
import org.scalajs.dom.raw.MouseEvent
import scalatags.JsDom.all._

import java.util.UUID

object OrganismActor {

  sealed trait Status {
    def color: String
  }

  case object Alive extends Status {
    override def color: String = "black"
  }

  case object Dead extends Status {
    override def color: String = "#DDDDDD"
  }

  /** [[neighbors]] are the 8 nearest neighbors */
  case class State(id: UUID, status: Status, neighbors: Set[UUID])

  sealed trait Command

  case object Die extends Command

  case object Live extends Command

  case object Toggle extends Command

  case class ReportState(sender: ActorRef[State]) extends Command

  object State {
    def oppositeOf(state: Status): Status = state match {
      case Alive => Dead
      case Dead => Alive
    }
  }

  case class Config(id: UUID, x: Int, y: Int, size: Int)

  def apply(config: Config, initialStatus: Status, neighbors: Set[UUID]): Behavior[Command] = {

    Behaviors.setup { context =>

      val rendered = div(
        position := "absolute",
        top := config.y,
        left := config.x,
        width := config.size,
        height := config.size
      ).render

      // change the organism's state when the user clicks (or taps, on mobile) on it.
      rendered.addEventListener("click", (_: MouseEvent) => {
        context.self ! Toggle
      })

      // add the rendered organism (just a colored square) to the document
      document.body.appendChild(rendered)

      def become(status: Status): Behavior[Command] = {
        import State._

        rendered.style.backgroundColor = status.color

        Behaviors.receiveMessage {
          case Die => become(Dead)
          case Live => become(Alive)
          case Toggle => become(oppositeOf(status))
          case ReportState(sender) =>
            sender ! State(config.id, status, neighbors)
            Behaviors.same
        }
      }

      become(initialStatus)
    }
  }

}
