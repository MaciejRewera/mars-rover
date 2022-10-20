package itv.mars.rover

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ShortestPathCalculatorSpec extends AnyWordSpec with Matchers {

  private val gridSize = 10

  "ShortestPathCalculator on shortestPath" when {

    "provided with the same start and target coordinates" should {
      "return single start/target coordinates" in {
        val start = Coordinates(1, 1)
        val target = Coordinates(1, 1)

        ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start)
      }
    }

    "provided with target being in a neighbour cell to start cell" should {

      "return only start and target Coordinates" when {

        "target is to the North" in {
          val start = Coordinates(1, 1)
          val target = Coordinates(1, 2)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }

        "target is to the South" in {
          val start = Coordinates(1, 1)
          val target = Coordinates(1, 0)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }

        "target is to the East" in {
          val start = Coordinates(1, 1)
          val target = Coordinates(2, 1)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }

        "target is to the West" in {
          val start = Coordinates(1, 1)
          val target = Coordinates(0, 1)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }
      }
    }

    "provided with start cell at the border of the grid and target at the opposite border of the grid" should {

      "return only start and target Coordinates" when {

        "start is at the bottom and target is at the top" in {
          val start = Coordinates(1, 0)
          val target = Coordinates(1, gridSize - 1)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }

        "start is at the top and target is at the bottom" in {
          val start = Coordinates(1, gridSize - 1)
          val target = Coordinates(1, 0)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }

        "start is at the left border and target is at the right border" in {
          val start = Coordinates(0, 1)
          val target = Coordinates(gridSize - 1, 1)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }

        "start is at the right border and target is at the left border" in {
          val start = Coordinates(gridSize - 1, 1)
          val target = Coordinates(0, 1)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(start, target)
        }
      }
    }

    "provided with start and target coordinates in the same vertical line" should {

      "return list of Coordinates in a straight line between the cells" when {

        "the shortest path runs through the grid to the North" in {
          val start = Coordinates(5, 1)
          val target = Coordinates(5, 5)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(5, 2),
            Coordinates(5, 3),
            Coordinates(5, 4),
            target
          )
        }

        "the shortest path runs through the grid to the South" in {
          val start = Coordinates(5, 5)
          val target = Coordinates(5, 1)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(5, 4),
            Coordinates(5, 3),
            Coordinates(5, 2),
            target
          )
        }

        "the shortest path runs through the grid's border to the North" in {
          val start = Coordinates(5, 7)
          val target = Coordinates(5, 1)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(5, 8),
            Coordinates(5, 9),
            Coordinates(5, 0),
            target
          )
        }

        "the shortest path runs through the grid's border to the South" in {
          val start = Coordinates(5, 1)
          val target = Coordinates(5, 7)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(5, 0),
            Coordinates(5, 9),
            Coordinates(5, 8),
            target
          )
        }
      }
    }

    "provided with start and target coordinates in the same horizontal line" should {

      "return list of Coordinates in a straight line between the cells" when {

        "the shortest path runs through the grid to the East" in {
          val start = Coordinates(1, 5)
          val target = Coordinates(5, 5)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(2, 5),
            Coordinates(3, 5),
            Coordinates(4, 5),
            target
          )
        }

        "the shortest path runs through the grid to the West" in {
          val start = Coordinates(5, 5)
          val target = Coordinates(1, 5)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(4, 5),
            Coordinates(3, 5),
            Coordinates(2, 5),
            target
          )
        }

        "the shortest path runs through the grid's border to the East" in {
          val start = Coordinates(7, 5)
          val target = Coordinates(1, 5)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(8, 5),
            Coordinates(9, 5),
            Coordinates(0, 5),
            target
          )
        }

        "the shortest path runs through the grid's border to the West" in {
          val start = Coordinates(1, 5)
          val target = Coordinates(7, 5)

          ShortestPathCalculator.shortestPath(start, target, gridSize) mustBe List(
            start,
            Coordinates(0, 5),
            Coordinates(9, 5),
            Coordinates(8, 5),
            target
          )
        }
      }
    }

    "provided with start and target coordinates somewhere on the grid" should {

      "return list of cell coordinates that form the shortest path from start to target" when {

        "the shortest path is equal to 2 cells" in {
          val start = Coordinates(1, 1)
          val target = Coordinates(2, 2)

          val result = ShortestPathCalculator.shortestPath(start, target, gridSize)

          result.length mustBe 3
          result.head mustBe start
          result.last mustBe target

          val potentialShortestPaths = List(
            List(start, Coordinates(1, 2), target),
            List(start, Coordinates(2, 1), target)
          )

          potentialShortestPaths must contain(result)
        }

        "the shortest path is equal to 3 cells" in {
          val start = Coordinates(1, 1)
          val target = Coordinates(3, 2)

          val result = ShortestPathCalculator.shortestPath(start, target, gridSize)

          result.length mustBe 4
          result.head mustBe start
          result.last mustBe target

          val potentialShortestPaths = List(
            List(start, Coordinates(1, 2), Coordinates(2, 2), target),
            List(start, Coordinates(2, 1), Coordinates(3, 1), target),
            List(start, Coordinates(2, 1), Coordinates(2, 2), target)
          )

          potentialShortestPaths must contain(result)
        }

        "the shortest path is equal to 4 cells" in {
          val start = Coordinates(1, 1)
          val target = Coordinates(3, 3)

          val result = ShortestPathCalculator.shortestPath(start, target, gridSize)

          result.length mustBe 5
          result.head mustBe start
          result.last mustBe target

          val potentialShortestPaths = List(
            List(start, Coordinates(1, 2), Coordinates(1, 3), Coordinates(2, 3), target),
            List(start, Coordinates(1, 2), Coordinates(2, 2), Coordinates(2, 3), target),
            List(start, Coordinates(1, 2), Coordinates(2, 2), Coordinates(3, 2), target),
            List(start, Coordinates(2, 1), Coordinates(3, 1), Coordinates(3, 2), target),
            List(start, Coordinates(2, 1), Coordinates(2, 2), Coordinates(3, 2), target),
            List(start, Coordinates(2, 1), Coordinates(2, 2), Coordinates(2, 3), target)
          )

          potentialShortestPaths must contain(result)
        }

      }
    }

    // TODO: How to handle situation where gridSize is an even number and there are 2 or more shortest paths (through the border)?
  }

}
