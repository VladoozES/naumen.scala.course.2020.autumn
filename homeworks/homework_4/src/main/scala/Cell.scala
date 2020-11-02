import scala.util.control.Breaks._

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
  var value: Option[Cell] = table.getCell(ix, iy)
  var is_cyclic = false
  var temp = table.getCell(ix, iy)
  breakable {
    while (temp.isInstanceOf[ReferenceCell]) {
      if (temp == value) {
        is_cyclic = true
        break
      }
      temp = temp.asInstanceOf[ReferenceCell].value
    }
  }
  override def toString: String = {
    if (is_cyclic) "cyclic"
    value.toString
  }
}
