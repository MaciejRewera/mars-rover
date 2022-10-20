package itv.mars.rover

case class Coordinates(x: Int, y: Int) {
  def incrX(gridSize: Int): Coordinates = this.copy(x = withRespectToGridSize(x + 1, gridSize))
  def decrX(gridSize: Int): Coordinates = this.copy(x = withRespectToGridSize(x - 1, gridSize))
  def incrY(gridSize: Int): Coordinates = this.copy(y = withRespectToGridSize(y + 1, gridSize))
  def decrY(gridSize: Int): Coordinates = this.copy(y = withRespectToGridSize(y - 1, gridSize))

  private def withRespectToGridSize(value: Int, gridSize: Int): Int = (value + gridSize) % gridSize
}
