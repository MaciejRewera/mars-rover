package itv.mars.rover

import itv.mars.rover.MoveCommand.{MoveForward, RotateAnticlockwise, RotateClockwise}
import itv.mars.rover.Orientation.North
import org.mockito.ArgumentMatchers.{any, eq => meq}
import org.mockito.MockitoSugar.{mock, reset, verify, when}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RoverSpec extends AnyWordSpec with Matchers with BeforeAndAfterEach {

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
        val rover = Rover.initial
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

  private val coordinates = mock[Coordinates]
  private val testCoordinates = Coordinates(1, 1, North)

  override def beforeEach(): Unit = {
    reset(coordinates)
    when(coordinates.applyCommand(any())).thenReturn(testCoordinates)
  }

  "Rover on runSingleCommand" when {

    "commands queue is empty" should {
      "return itself" in {
        val rover = Rover.initial

        val result = rover.runSingleCommand()

        result mustBe rover
      }
    }

    "commands queue contains single command" should {

      "NOT change the original Rover" in {
        val command: MoveCommand = RotateClockwise
        val rover = Rover(coordinates, command)

        rover.runSingleCommand()

        rover.commands.toList mustBe List(command)
        rover.coordinates mustBe coordinates
      }

      "return Rover with empty commands queue" in {
        val command: MoveCommand = RotateClockwise
        val rover = Rover(coordinates, command)

        val result = rover.runSingleCommand()

        result.commands mustBe empty
      }

      "call Coordinates applyCommand method" in {
        val command: MoveCommand = RotateClockwise
        val rover = Rover(coordinates, command)

        rover.runSingleCommand()

        verify(coordinates).applyCommand(meq(command))
      }

      "return Rover with coordinates obtained from Coordinates applyCommand method" in {
        val command: MoveCommand = RotateClockwise
        val rover = Rover(coordinates, command)

        val result = rover.runSingleCommand()

        result.coordinates mustBe testCoordinates
      }
    }

    "commands queue contains multiple commands" should {

      "return Rover with commands queue without first element" in {
        val commands: List[MoveCommand] = List(RotateClockwise, MoveForward, RotateAnticlockwise)
        val rover = Rover(coordinates, commands: _*)

        val result = rover.runSingleCommand()

        result.commands.toList mustBe commands.tail
      }

      "return Rover with coordinates obtained from Coordinates applyCommand method" in {
        val commands: List[MoveCommand] = List(RotateClockwise, MoveForward, RotateAnticlockwise)
        val rover = Rover(coordinates, commands: _*)

        val result = rover.runSingleCommand()

        result.coordinates mustBe testCoordinates
      }
    }
  }

}
