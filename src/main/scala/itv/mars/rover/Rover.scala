package itv.mars.rover

import scala.collection.immutable.Queue


case class Rover private(commands: Queue[MoveCommand]) {

  def issueCommand(newCommand: MoveCommand): Rover = Rover(commands.enqueue(newCommand))
}

object Rover {
  def empty: Rover = Rover(Queue.empty)

  def apply(moveCommands: MoveCommand*): Rover = Rover(Queue.apply(moveCommands: _*))
}