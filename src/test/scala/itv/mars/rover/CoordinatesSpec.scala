package itv.mars.rover

import itv.mars.rover.MoveCommand._
import itv.mars.rover.Orientation.{East, North, South, West}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CoordinatesSpec extends AnyWordSpec with Matchers {

  private def testCoordinates(orientation: Orientation) = Coordinates(1, 1, orientation)

  private val faceNorth = testCoordinates(North)
  private val faceSouth = testCoordinates(South)
  private val faceEast = testCoordinates(East)
  private val faceWest = testCoordinates(West)

  "Coordinates on applyCommand" when {

    "provided with MoveForward command" when {

      val command = MoveForward

      "orientation is North" should {
        "return Coordinates with increased y coordinate" in {
          faceNorth.applyCommand(command) mustBe faceNorth.copy(y = 2)
        }
      }

      "orientation is South" should {
        "return Coordinates with decreased y coordinate" in {
          faceSouth.applyCommand(command) mustBe faceSouth.copy(y = 0)
        }
      }

      "orientation is East" should {
        "return Coordinates with increased x coordinate" in {
          faceEast.applyCommand(command) mustBe faceEast.copy(x = 2)
        }
      }

      "orientation is West" should {
        "return Coordinates with decreased x coordinate" in {
          faceWest.applyCommand(command) mustBe faceWest.copy(x = 0)
        }
      }
    }

    "provided with RotateClockwise command" when {

      val command = RotateClockwise

      "orientation is North" should {
        "return Coordinates with East orientation" in {
          faceNorth.applyCommand(command) mustBe faceNorth.copy(orientation = East)
        }
      }

      "orientation is South" should {
        "return Coordinates with West orientation" in {
          faceSouth.applyCommand(command) mustBe faceSouth.copy(orientation = West)
        }
      }

      "orientation is East" should {
        "return Coordinates with South orientation" in {
          faceEast.applyCommand(command) mustBe faceEast.copy(orientation = South)
        }
      }

      "orientation is West" should {
        "return Coordinates with North orientation" in {
          faceWest.applyCommand(command) mustBe faceWest.copy(orientation = North)
        }
      }
    }

    "provided with RotateAnticlockwise command" when {

      val command = RotateAnticlockwise

      "orientation is North" should {
        "return Coordinates with West orientation" in {
          faceNorth.applyCommand(command) mustBe faceNorth.copy(orientation = West)
        }
      }

      "orientation is South" should {
        "return Coordinates with East orientation" in {
          faceSouth.applyCommand(command) mustBe faceSouth.copy(orientation = East)
        }
      }

      "orientation is East" should {
        "return Coordinates with North orientation" in {
          faceEast.applyCommand(command) mustBe faceEast.copy(orientation = North)
        }
      }

      "orientation is West" should {
        "return Coordinates with South orientation" in {
          faceWest.applyCommand(command) mustBe faceWest.copy(orientation = South)
        }
      }
    }

  }

}
