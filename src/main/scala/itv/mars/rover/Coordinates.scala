package itv.mars.rover

import itv.mars.rover.MoveCommand.{MoveForward, RotateAnticlockwise, RotateClockwise}
import itv.mars.rover.Orientation.{East, North, South, West}

case class Coordinates(x: Int, y: Int, orientation: Orientation, gridSize: Int) {

  def applyCommand(command: MoveCommand): Coordinates = command match {
    case MoveForward         => applyMoveForward
    case RotateClockwise     => applyRotateClockwise
    case RotateAnticlockwise => applyRotateAnticlockwise
  }

  private def applyMoveForward: Coordinates = orientation match {
    case North => this.copy(y = withRespectToGridSize(y + 1))
    case South => this.copy(y = withRespectToGridSize(y - 1))
    case East  => this.copy(x = withRespectToGridSize(x + 1))
    case West  => this.copy(x = withRespectToGridSize(x - 1))
  }

  private val withRespectToGridSize: Int => Int = a => (a + gridSize) % gridSize

  private val directionsInClockwiseOrder = List(North -> East, East -> South, South -> West, West -> North)
  private val directionsInAnticlockwiseOrder = directionsInClockwiseOrder.map(_.swap)

  private def applyRotateClockwise: Coordinates =
    this.copy(orientation = directionsInClockwiseOrder.filter(_._1 == this.orientation).head._2)

  private def applyRotateAnticlockwise: Coordinates =
    this.copy(orientation = directionsInAnticlockwiseOrder.filter(_._1 == this.orientation).head._2)
}

object Coordinates {
  def zeros: Coordinates = Coordinates(0, 0, North, 1)

  def main(args: Array[String]): Unit = {

    val directions = Vector(North, East, South, West)
    val list = LazyList.unfold(0)(i => Some(directions(i % 4), (i + 1) % 4))
    println(list.take(123)
      .grouped(4)
      .map(_.toList)
      .foldRight(List.empty[List[Orientation]])((l, acc) => if (acc.headOption.contains(l)) acc else l :: acc)
      .mkString("\n"))
  }
}
