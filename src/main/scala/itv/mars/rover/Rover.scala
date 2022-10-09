package itv.mars.rover

import scala.collection.immutable.Queue

case class Rover private (coordinates: Coordinates, commands: Queue[MoveCommand]) {

  def issueCommand(newCommand: MoveCommand): Rover = this.copy(commands = commands.enqueue(newCommand))

  def runSingleCommand(): Rover =
    if (commands.isEmpty) this
    else {
      val (command, commandsLeft) = commands.dequeue
      Rover(
        commands = commandsLeft,
        coordinates = coordinates.applyCommand(command)
      )
    }

}

object Rover {
  lazy val initial: Rover = Rover(Coordinates.zeros, Queue.empty)

  def apply(moveCommands: MoveCommand*): Rover = Rover(Coordinates.zeros, Queue.apply(moveCommands: _*))

  def apply(coordinates: Coordinates): Rover = Rover(coordinates, Queue.empty)

  def apply(coordinates: Coordinates, moveCommands: MoveCommand*): Rover =
    Rover(coordinates, Queue.apply(moveCommands: _*))
}
