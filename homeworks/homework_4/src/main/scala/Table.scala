class Table(var x: Int, var y: Int) {
  private val width = x
  private val height = y
  var matrix = new Array[Cell](x*y)
  for (i <- 0 until x*y) {
    matrix(i) = new EmptyCell
  }

  def getCell(ix: Int, iy: Int): Option[Cell] = {
    if (ix >= width || ix >= height) return None
    Option(matrix(ix + iy * width))
  }

  def setCell(ix: Int, iy: Int, cell: Cell): Unit = {
    matrix(ix + iy * width) = cell
  }
}
