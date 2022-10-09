package itv.mars.rover

import itv.mars.rover.MoveCommand.{MoveForward, RotateAnticlockwise, RotateClockwise}
import itv.mars.rover.Orientation.{East, North, South, West}

case class Coordinates(x: Int, y: Int, orientation: Orientation) {

  def applyCommand(command: MoveCommand): Coordinates = command match {
    case MoveForward         => applyMoveForward
    case RotateClockwise     => applyRotateClockwise
    case RotateAnticlockwise => applyRotateAnticlockwise
  }

  private def applyMoveForward: Coordinates = orientation match {
    case North => this.copy(y = y + 1)
    case South => this.copy(y = y - 1)
    case East  => this.copy(x = x + 1)
    case West  => this.copy(x = x - 1)
  }

  private val directionsInClockwiseOrder = List(North -> East, East -> South, South -> West, West -> North)
  private val directionsInAnticlockwiseOrder = directionsInClockwiseOrder.map(_.swap)

  private def applyRotateClockwise: Coordinates =
    this.copy(orientation = directionsInClockwiseOrder.filter(_._1 == this.orientation).head._2)

  private def applyRotateAnticlockwise: Coordinates =
    this.copy(orientation = directionsInAnticlockwiseOrder.filter(_._1 == this.orientation).head._2)
}

object Coordinates {
  def zeros: Coordinates = Coordinates(0, 0, North)
}
