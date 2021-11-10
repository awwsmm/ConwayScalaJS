package conway.actors

import akka.actor.{Actor, Props}
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.MouseEvent
import scalatags.JsDom.all._

case class OrganismActor(init: OrganismActor.State, logging: Boolean) extends Actor {
  import OrganismActor._

  private[this] var state: State = init

  override def receive: Receive = {
    case Die =>
      if (logging) println(s"organism at (${state.x}, ${state.y}) received command to Die")
      state = state.copy(deadOrAlive = Dead)
      reRender()

    case Live =>
      if (logging) println(s"organism at (${state.x}, ${state.y}) received command to Live")
      state = state.copy(deadOrAlive = Alive)
      reRender()

    case Toggle =>
      if (logging) println(s"organism at (${state.x}, ${state.y}) was Toggled")
      state = state.copy(deadOrAlive = toggle(state.deadOrAlive))
      reRender()

    case Poke =>
      sender ! state
  }

  private def createChild(): Div = {
    val color = if (state.deadOrAlive == Alive) "black" else "#DDDDDD"

    val rendered = div(
      position := "absolute",
      top := state.y - state.size / 2,
      left := state.x - state.size / 2,
      width := state.size,
      height := state.size,
      backgroundColor := color
    ).render

    rendered.addEventListener("click", (_: MouseEvent) => {
      self ! Toggle
    })

    rendered
  }

  private[this] var child = createChild()

  override def preStart(): Unit = {
    super.preStart()
    document.body.appendChild(child)
  }

  private def reRender(): Unit = {
    child.style.backgroundColor = if (state.deadOrAlive == Alive) "black" else "#DDDDDD"
  }
}

object OrganismActor {

  sealed trait DeadOrAlive

  case object Dead extends DeadOrAlive

  case object Alive extends DeadOrAlive

  sealed trait Command

  case object Die extends Command

  case object Live extends Command

  case object Toggle extends Command

  def toggle(deadOrAlive: DeadOrAlive): DeadOrAlive = deadOrAlive match {
    case Dead => Alive
    case Alive => Dead
  }

  sealed trait Query

  case object Poke extends Query

  case class State(deadOrAlive: DeadOrAlive, x: Double, y: Double, size: Double)

  def props(deadOrAlive: DeadOrAlive, x: Double, y: Double, size: Double, logging: Boolean): Props =
    Props(new OrganismActor(State(deadOrAlive, x, y, size), logging))
}