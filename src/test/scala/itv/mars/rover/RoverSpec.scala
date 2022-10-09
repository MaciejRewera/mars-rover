package itv.mars.rover

import itv.mars.rover.MoveCommand.{MoveForward, RotateAnticlockwise, RotateClockwise}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RoverSpec extends AnyWordSpec with Matchers {

  "Rover on issueCommand" should {

    "NOT change the original Rover" in {
      val previousCommands: List[MoveCommand] = List(RotateClockwise, MoveForward, RotateAnticlockwise)
      val newCommand: MoveCommand = MoveForward
      val rover = Rover(previousCommands: _*)

      rover.issueCommand(newCommand)

      rover.commands.toList mustBe previousCommands
    }

    "return new Rover with the new command added at the end of the commands queue" when {

      "there were no commands issued before" in {
        val rover = Rover.empty
        val newCommand: MoveCommand = MoveForward

        val newRover = rover.issueCommand(newCommand)

        newRover.commands.toList mustBe List(newCommand)
      }

      "there was a single command in the queue" in {
        val previousCommand: MoveCommand = RotateClockwise
        val newCommand: MoveCommand = MoveForward
        val rover = Rover(previousCommand)

        val newRover = rover.issueCommand(newCommand)

        newRover.commands.toList mustBe List(previousCommand, newCommand)
      }

      "there were multiple commands in the queue" in {
        val previousCommands: List[MoveCommand] = List(RotateClockwise, MoveForward, RotateAnticlockwise)
        val newCommand: MoveCommand = MoveForward
        val rover = Rover(previousCommands: _*)

        val newRover = rover.issueCommand(newCommand)

        val expectedCommands = previousCommands :+ newCommand
        newRover.commands.toList mustBe expectedCommands
      }
    }
  }

}
