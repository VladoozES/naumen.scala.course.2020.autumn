trait Cell {

}

class EmptyCell extends Cell {
  override def toString: String = "empty"
}

class NumberCell(number: Int) extends Cell {
  val value: Int = number

  override def toString: String = number.toString
}

class StringCell(str: String) extends Cell {
  val value: String = str

  override def toString: String = value
}

class ReferenceCell(ix: Int, iy: Int, table: Table) extends Cell {
  val index_x = ix
  val index_y = iy
  val source = table

  override def toString: String = source.getCell(ix, iy).toString
}
