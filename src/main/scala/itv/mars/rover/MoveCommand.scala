package itv.mars.rover

sealed trait MoveCommand

object MoveCommand {
  case object MoveForward extends MoveCommand
  case object RotateClockwise extends MoveCommand
  case object RotateAnticlockwise extends MoveCommand
}
