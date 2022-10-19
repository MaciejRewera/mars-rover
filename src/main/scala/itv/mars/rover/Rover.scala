package itv.mars.rover

import cats.data.State
import itv.mars.rover.MoveCommand._
import itv.mars.rover.Orientation._

case class Rover(coordinates: Coordinates, orientation: Orientation, gridSize: Int) {
  private def incrX: Rover = this.copy(coordinates = coordinates.copy(x = withRespectToGridSize(coordinates.x + 1)))
  private def decrX: Rover = this.copy(coordinates = coordinates.copy(x = withRespectToGridSize(coordinates.x - 1)))
  private def incrY: Rover = this.copy(coordinates = coordinates.copy(y = withRespectToGridSize(coordinates.y + 1)))
  private def decrY: Rover = this.copy(coordinates = coordinates.copy(y = withRespectToGridSize(coordinates.y - 1)))

  private def withRespectToGridSize(value: Int): Int = (value + gridSize) % gridSize
}

object Rover {

  def runCommands(commands: List[MoveCommand]): State[Rover, Unit] = {
    for {
      _ <- commands.foldLeft(State.empty[Rover, Unit])((currState, cmd) => currState.modify(move(cmd)))
      res <- State.get
    } yield res
  }

  private def move(command: MoveCommand) = (rover: Rover) => command match {
    case MoveForward => applyMoveForward(rover)
    case RotateClockwise => applyRotateClockwise(rover)
    case RotateAnticlockwise => applyRotateAnticlockwise(rover)
  }

  private val applyMoveForward = (rover: Rover) => rover.orientation match {
    case North => rover.incrY
    case South => rover.decrY
    case East  => rover.incrX
    case West  => rover.decrX
  }

  private val applyRotateClockwise = (rover: Rover) => rover.orientation match {
    case North => rover.copy(orientation = East)
    case South => rover.copy(orientation = West)
    case East  => rover.copy(orientation = South)
    case West  => rover.copy(orientation = North)
  }

  private val applyRotateAnticlockwise = (rover: Rover) => rover.orientation match {
    case North => rover.copy(orientation = West)
    case South => rover.copy(orientation = East)
    case East  => rover.copy(orientation = North)
    case West  => rover.copy(orientation = South)
  }
}
