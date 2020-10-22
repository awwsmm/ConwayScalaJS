package conway.actors

import akka.actor.{Actor, Props}
import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.MouseEvent
import scalatags.JsDom.all._

case class OrganismActor(init: OrganismActor.State) extends Actor {
  import OrganismActor._

  private[this] var state: State = init

  override def receive: Receive = {
    case Die =>
      println(s"organism at (${state.x}, ${state.y}) received command to Die")
      state = state.copy(deadOrAlive = Dead)
      reRender()

    case Live =>
      println(s"organism at (${state.x}, ${state.y}) received command to Live")
      state = state.copy(deadOrAlive = Alive)
      reRender()

    case Toggle =>
      println(s"organism at (${state.x}, ${state.y}) was Toggled")
      state = state.copy(deadOrAlive = toggle(state.deadOrAlive))
      reRender()

    case Poke =>
      sender ! state
  }

  private def createChild(): Div = {
    val color = if (state.deadOrAlive == Alive) "black" else "white"

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
    val newChild = createChild()
    document.body.replaceChild(newChild, child)
    child = newChild
  }
}

object OrganismActor {

  sealed trait DeadOrAlive

  object Dead extends DeadOrAlive {
    override def toString: String = "Dead"
  }

  object Alive extends DeadOrAlive {
    override def toString: String = "Alive"
  }

  sealed trait Command

  object Die extends Command {
    override def toString: String = "Die"
  }

  object Live extends Command {
    override def toString: String = "Live"
  }

  object Toggle extends Command

  def toggle(deadOrAlive: DeadOrAlive): DeadOrAlive = deadOrAlive match {
    case Dead => Alive
    case Alive => Dead
  }

  sealed trait Query

  object Poke extends Query

  case class State(deadOrAlive: DeadOrAlive, x: Double, y: Double, size: Double)

  def props(deadOrAlive: DeadOrAlive, x: Double, y: Double, size: Double): Props =
    Props(new OrganismActor(State(deadOrAlive, x, y, size)))
}