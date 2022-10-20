package itv.mars.rover

import scala.collection.immutable.Queue

object ShortestPathCalculator {

  def shortestPath(start: Coordinates, target: Coordinates, gridSize: Int): List[Coordinates] = {
    def loop(
        queue: Queue[Coordinates],
        visited: Set[Coordinates],
        previousNodes: Map[Coordinates, Coordinates]
    ): Map[Coordinates, Coordinates] =
      if (queue.nonEmpty) {
        val (currCell, queueLeft) = queue.dequeue

        if (!visited(currCell)) {
          if (currCell == target)
            previousNodes
          else {
            val neighbours = neighboursForCell(currCell, gridSize).filterNot(n => visited(n) || queue.contains(n))
            val newQueue = queueLeft ++ neighbours

            val toAddToPreviousNodes = neighbours.map(n => (n, currCell))
            val newPreviousNodes =
              toAddToPreviousNodes.foldLeft(previousNodes)((nodes, n) => if (nodes.contains(n._1)) nodes else nodes + n)

            loop(newQueue, visited + currCell, newPreviousNodes)
          }
        } else {
          loop(queueLeft, visited, previousNodes)
        }
      } else {
        Map.empty
      }

    val previousNodes = loop(Queue(start), Set.empty[Coordinates], Map.empty)
    deconstructPath(target, previousNodes).reverse
  }

  private def neighboursForCell(cell: Coordinates, gridSize: Int): List[Coordinates] = List(
    cell.incrX(gridSize),
    cell.decrX(gridSize),
    cell.incrY(gridSize),
    cell.decrY(gridSize)
  )

  private def deconstructPath(currNode: Coordinates, previousNodes: Map[Coordinates, Coordinates]): List[Coordinates] =
    previousNodes.get(currNode) match {
      case None => currNode +: Nil
      case Some(node) => currNode +: deconstructPath(node, previousNodes)
    }

}
