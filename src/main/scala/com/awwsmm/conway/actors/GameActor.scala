package com.awwsmm.conway.actors

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

import java.util.UUID

object GameActor {

  case class Config(organismSize: Int, padding: Int, margin: Int)

  // TODO this can DEFINITELY be optimised
  case class Landscape (map: String) {
    private val lines = map.stripMargin.split('\n')
    val nRows: Int = lines.length
    require(nRows > 0, "must have at least one row")

    private val colsPerRow = lines.map(_.length)
    require(colsPerRow.distinct.length == 1, "all rows must have the same number of cols")

    val nCols: Int = colsPerRow(0)
    require(nCols > 0, "must have at least one col")

    require(lines.mkString("").matches("[_X]+"), "map can only contain _ and X")

    // TODO should be decoupled from OrganismActor?
    private val state = lines.map(line => line.map(x => if (x == 'X') (OrganismActor.Alive, UUID.randomUUID()) else (OrganismActor.Dead, UUID.randomUUID())))

    def apply(row: Int, col: Int): (OrganismActor.Status, UUID) = {
      state(Math.floorMod(row, nRows))(Math.floorMod(col, nCols))
    }

    override def toString: String = {
      "GameMap(\n" + lines.mkString("  ", "\n  ", "") + "\n)"
    }
  }

  sealed trait Command

  case object Tick extends Command

  case class OrganismState(state: OrganismActor.State) extends Command

  case object Toggle extends Command

  def apply(config: Config, landscape: Landscape): Behavior[Command] = {

    Behaviors.setup { context =>

      val organisms: Map[UUID, ActorRef[OrganismActor.Command]] = {

        def colToX (col: Int): Int = config.margin + (col * (config.organismSize + config.padding))
        def rowToY (row: Int): Int = config.margin + (row * (config.organismSize + config.padding))

        def findNeighbors(row: Int, col: Int): Set[UUID] = {
          (for {
            rowDelta <- -1 to 1
            colDelta <- -1 to 1
            if !(rowDelta == 0 && colDelta == 0)
            (_, uuid) = landscape(row+rowDelta, col+colDelta)
          } yield {
            uuid
          }).toSet
        }

        (for {
          row <- 0 until landscape.nRows
          col <- 0 until landscape.nCols
          (x, y, (status, uuid), neighbors) = (colToX(col), rowToY(row), landscape(row, col), findNeighbors(row, col))
        } yield {
          val organismConfig = OrganismActor.Config(uuid, x, y, config.organismSize)
          uuid -> context.spawn(OrganismActor(organismConfig, status, neighbors), uuid.toString)
        }).toMap

      }

      val organismStateAdapter: ActorRef[OrganismActor.State] =
        context.messageAdapter(organismState => OrganismState(organismState))

      def continue(landscape: Map[UUID, OrganismActor.State], automatic: Boolean): Behavior[Command] = {

        // if we know the entire current landscape, calculate the next landscape
        if (landscape.keySet == organisms.keySet) {
          landscape.foreach { case (_, OrganismActor.State(uuid, status, neighbors)) =>

            val nAliveNeighbors = neighbors.map(landscape).count(_.status == OrganismActor.Alive)

            if (status == OrganismActor.Alive) {

              // this organism dies from underpopulation
              if (nAliveNeighbors < 2) organisms(uuid) ! OrganismActor.Die

              // this organism dies from overpopulation
              else if (nAliveNeighbors > 3) organisms(uuid) ! OrganismActor.Die

            } else if (nAliveNeighbors == 3) {

              // a new organism is born
              organisms(uuid) ! OrganismActor.Live

            }
          }

          // if the landscape is updating automatically, calculate the next one immediately
          if (automatic) context.self ! Tick
          continue(Map.empty, automatic)

        } else {

          Behaviors.receiveMessage {
            case Tick =>
              organisms.foreach { case (_, ref) => ref ! OrganismActor.ReportState(organismStateAdapter) }
              Behaviors.same
            case OrganismState(organismState) =>
              continue(landscape + (organismState.id -> organismState), automatic)
            case Toggle =>
              if (!automatic) context.self ! Tick
              continue(landscape, !automatic)
          }
        }
      }

      continue(Map.empty, false)
    }
  }

}
