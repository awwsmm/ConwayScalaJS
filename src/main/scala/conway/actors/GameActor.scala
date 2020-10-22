package conway.actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

case class GameActor(state: GameActor.State) extends Actor {

  private[this] var landscape = Map[ActorRef, OrganismActor.State]()

  override def receive: Receive = {
    case GameActor.Tick =>
      for (((row, col), organism) <- state.organisms) {
        organism ! OrganismActor.Poke
      }

    case orgState@OrganismActor.State(deadOrAlive, x, y, _) =>
      landscape += (sender -> orgState)
//      println(s"organism #${landscape.size} at ($x, $y) is $deadOrAlive")
      calculateNextLandscape()

  }

  private def colWithWrapping (col: Int) = Math.floorMod(col, state.gameMap.nCols)
  private def rowWithWrapping (row: Int) = Math.floorMod(row, state.gameMap.nRows)

  private def nearestNeighbors(organism: ActorRef, orgState: OrganismActor.State): Set[(ActorRef, OrganismActor.DeadOrAlive)] = {

    val OrganismActor.State(_, x, y, _) = orgState

    val thisCol = state.xToCol(x)
    val thisRow = state.yToRow(y)

//    println(s"searching for neighbors of organism @ ($thisRow, $thisCol)")

    // since we know the landscape is a grid, convert (x, y) to (row, col)
    val rowColMap = for {
      (orgRef, orgState) <- landscape
      OrganismActor.State(otherDorA, x, y, _) = orgState
    } yield (state.yToRow(y), state.xToCol(x)) -> (orgRef, otherDorA)

    def printRowColMap(): Unit = println(
      rowColMap.groupBy({
        case ((row, _), _) => row
      }).map({
        case (_, mapOfRow) =>
          mapOfRow.map({
            case ((_, col), (_, doa)) =>
              val s = if (doa == OrganismActor.Dead) "_" else "X"
              (col, s)
          }).toList.sorted.map(_._2).mkString("")
      }).mkString("\n")
    )

    val colLeft  = colWithWrapping(thisCol - 1)
    val colRight = colWithWrapping(thisCol + 1)
    val rowAbove = rowWithWrapping(thisRow - 1)
    val rowBelow = rowWithWrapping(thisRow + 1)

    Set(
      rowColMap(rowAbove, colLeft),
      rowColMap(rowAbove, thisCol),
      rowColMap(rowAbove, colRight),

      rowColMap(thisRow, colLeft),
      rowColMap(thisRow, colRight),

      rowColMap(rowBelow, colLeft),
      rowColMap(rowBelow, thisCol),
      rowColMap(rowBelow, colRight)
    )
  }

  private def calculateNextLandscape() = {
    if (landscape.size == state.organisms.size) {

      for {
        (orgRef, orgState) <- landscape
        OrganismActor.State(deadOrAlive, x, y, _) = orgState
//        _ = println(s"actor @ ($x, $y)")
        neighborStates = nearestNeighbors(orgRef, orgState).toList.map(_._2)
//        _ = println(s"the neighbors of ($x, $y) [$deadOrAlive] are $neighborStates")
      } yield {
        val nAliveNeighbors = neighborStates.count(_ == OrganismActor.Alive)
        val isAlive = orgState.deadOrAlive == OrganismActor.Alive

        println(s"organism at ($x, $y) is ${orgState.deadOrAlive} and has $nAliveNeighbors/${neighborStates.size} living neighbors")

        val optCommand = {
          if (isAlive) {

            // this organism dies from underpopulation
            if (nAliveNeighbors < 2) Some(OrganismActor.Die)

            // this organism dies from overpopulation
            else if (nAliveNeighbors > 3) Some(OrganismActor.Die)

            else None

          // a new organism is born
          } else if (nAliveNeighbors == 3) {
            Some(OrganismActor.Live)

          } else None
        }

        optCommand match {
          case Some(command) =>
            println(s"organism at ($x, $y) will $command because it has $nAliveNeighbors living neighbors")
            orgRef ! command
          case None =>
        }


      }

      landscape = landscape.empty
    }
  }


}

object GameActor {

  def props(state: GameActor.State): Props = Props(new GameActor(state))

  case class GameMap (map: String) {
    private val lines = map.stripMargin.split('\n')
    val nRows: Int = lines.length
    require(nRows > 0, "must have at least one row")

    private val colsPerRow = lines.map(_.length)
    require(colsPerRow.distinct.length == 1, "all rows must have the same number of cols")

    val nCols: Int = colsPerRow(0)
    require(nCols > 0, "must have at least one col")

    require(lines.mkString("").matches("[_X]+"), "map can only contain _ and X")
    private val state = lines.map(line => line.map(x => if (x == 'X') OrganismActor.Alive else OrganismActor.Dead))

    def apply(row: Int, col: Int): OrganismActor.DeadOrAlive =
      state(Math.floorMod(row, nRows))(Math.floorMod(col, nCols))

    override def toString: String = {
      "GameMap(\n" + lines.mkString("  ", "\n  ", "") + "\n)"
    }
  }

  case class State (gameMap: GameMap, organismSize: Int, padding: Int, margin: Int)(implicit system: ActorSystem) {
    lazy val organisms: Map[(Int, Int), ActorRef] = (
      for {
        col <- 0 until gameMap.nCols
        row <- 0 until gameMap.nRows
        x = colToX(col)
        y = rowToY(row)

      } yield {
        (row, col) -> system.actorOf(OrganismActor.props(gameMap(row, col), x, y, organismSize))
      }).toMap

    def yToRow (y: Double): Int = ((y - margin) / (organismSize + padding)).toInt
    def xToCol (x: Double): Int = ((x - margin) / (organismSize + padding)).toInt

    def colToX (col: Int): Int = margin + (col * (organismSize + padding))
    def rowToY (row: Int): Int = margin + (row * (organismSize + padding))
  }

  object Tick
}

