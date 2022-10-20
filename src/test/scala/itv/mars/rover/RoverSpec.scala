package itv.mars.rover

import itv.mars.rover.MoveCommand._
import itv.mars.rover.Orientation._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RoverSpec extends AnyWordSpec with Matchers {

  private val gridSize = 13

  "Rover on runCommands" when {

    "commands list is empty" should {
      "return itself" in {
        val commands = List.empty[MoveCommand]
        val rover = Rover(Coordinates(1, 1), North, gridSize)

        testCommands(commands, rover, rover)
      }
    }

    "commands list contains single MoveForward command" when {
      val commands = List(MoveForward)

      "orientation is North" when {

        "rover is NOT at the border of the grid" should {
          "return Rover with Y coordinate increased by 1" in {
            val rover = Rover(Coordinates(1, 1), North, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = 1, y = 2))

            testCommands(commands, rover, expectedRover)
          }
        }

        "rover is at the North border of the grid" should {
          "return Rover with Y coordinate equal to 0" in {
            val rover = Rover(Coordinates(1, gridSize - 1), North, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = 1, y = 0))

            testCommands(commands, rover, expectedRover)
          }
        }
      }

      "orientation is South" when {

        "rover is NOT at the border of the grid" should {
          "return Rover with Y coordinate decreased by 1" in {
            val rover = Rover(Coordinates(1, 1), South, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = 1, y = 0))

            testCommands(commands, rover, expectedRover)
          }
        }

        "rover is at the South border of the grid" should {
          "return Rover with Y coordinate equal to grid size minus 1" in {
            val gridSize = 13
            val rover = Rover(Coordinates(1, 0), South, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = 1, y = gridSize - 1))

            testCommands(commands, rover, expectedRover)
          }
        }
      }

      "orientation is East" when {

        "rover is NOT at the border of the grid" should {
          "return Rover with X coordinate increased by 1" in {
            val rover = Rover(Coordinates(1, 1), East, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = 2, y = 1))

            testCommands(commands, rover, expectedRover)
          }
        }

        "rover is at the East border of the grid" should {
          "return Rover with X coordinate equal to 0" in {
            val gridSize = 13
            val rover = Rover(Coordinates(gridSize - 1, 1), East, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = 0, y = 1))

            testCommands(commands, rover, expectedRover)
          }
        }
      }

      "orientation is West" when {

        "rover is NOT at the border of the grid" should {
          "return Rover with X coordinate decreased by 1" in {
            val rover = Rover(Coordinates(1, 1), West, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = 0, y = 1))

            testCommands(commands, rover, expectedRover)
          }
        }

        "rover is at the West border of the grid" should {
          "return Rover with X coordinate equal to grid size minus 1" in {
            val gridSize = 13
            val rover = Rover(Coordinates(0, 1), West, gridSize)
            val expectedRover = rover.copy(coordinates = Coordinates(x = gridSize - 1, y = 1))

            testCommands(commands, rover, expectedRover)
          }
        }
      }

    }

    "commands list contains single RotateClockwise command" when {
      val commands = List(RotateClockwise)

      "orientation is North" should {
        "return Rover with Orientation East" in {
          testRotateCommands(commands, North, East)
        }
      }

      "orientation is South" should {
        "return Rover with Orientation West" in {
          testRotateCommands(commands, South, West)
        }
      }

      "orientation is East" should {
        "return Rover with Orientation South" in {
          testRotateCommands(commands, East, South)
        }
      }

      "orientation is West" should {
        "return Rover with Orientation North" in {
          testRotateCommands(commands, West, North)
        }
      }
    }

    "commands list contains single RotateAnticlockwise command" when {
      val commands = List(RotateAnticlockwise)

      "orientation is North" should {
        "return Rover with Orientation West" in {
          testRotateCommands(commands, North, West)
        }
      }

      "orientation is South" should {
        "return Rover with Orientation East" in {
          testRotateCommands(commands, South, East)
        }
      }

      "orientation is East" should {
        "return Rover with Orientation North" in {
          testRotateCommands(commands, East, North)
        }
      }

      "orientation is West" should {
        "return Rover with Orientation South" in {
          testRotateCommands(commands, West, South)
        }
      }
    }
  }

  private def testRotateCommands(
      commands: List[MoveCommand],
      initialOrientation: Orientation,
      expectedOrientation: Orientation
  ): Unit = {
    val rover = Rover(Coordinates(1, 1), initialOrientation, gridSize)
    testCommands(commands, rover, rover.copy(orientation = expectedOrientation))
  }

  private def testCommands(
      commands: List[MoveCommand],
      initialRover: Rover,
      expectedRover: Rover
  ): Unit = {
    val actualRover = Rover.runCommands(commands).run(initialRover).value._1
    actualRover.coordinates mustBe expectedRover.coordinates
    actualRover.orientation mustBe expectedRover.orientation
  }

}
