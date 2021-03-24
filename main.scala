object main extends App {
  object Status extends Enumeration {
    type Status = Value
    val UNOCCUPIED, UNMASKED, MASKED, SICK = Value
  }

  import Status._

  case class Position(row: Int, col: Int)
  case class Seat(pos: Position, status: Status)

  def getSeats(matrix: List[List[Status]]) = {
    for ((row, i) <- matrix.zipWithIndex; (cell, j) <- row.zipWithIndex)
      yield Seat(Position(i, j), cell)
  }

  def getKernel(size: Int) = {
    val validSize = if (size < 3) 3 else if (size % 2 == 0) size + 1 else size
    for (
      (row, i) <- (1 to validSize).zipWithIndex;
      (cell, j) <- (1 to validSize).zipWithIndex
    ) yield ((i - (validSize / 2), j - validSize / 2))
  }

  // rewrite to accept map and output map
  def getAdjacent(
      pos: Position,
      kernel: IndexedSeq[(Int, Int)],
      list: List[List[Status]]
  ) = {
    kernel
      .map(k => Position(pos.row - k._1, pos.col - k._2))
      .filter(pos => pos.row >= 0 && pos.col >= 0)
      .filter(pos => pos.row < list.length && pos.col < list(0).length)
      .map(pos => (pos, list(pos.row)(pos.col)))
  }

  def getUpdate(
      seq: IndexedSeq[(Position, Status)]
  ): IndexedSeq[(Position, Status)] = {
    seq.map(pos => {
      pos._2 match {
        case SICK       => (pos._1, SICK)
        case UNMASKED   => (pos._1, SICK)
        case MASKED     => (pos._1, UNMASKED)
        case UNOCCUPIED => (pos._1, UNOCCUPIED)
      }
    })
  }

  val initial = List(
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, SICK, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, SICK, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED)
  )

  val seats = getSeats(initial)
  val kernel = getKernel(3)
  val sick = seats.filter(seat => seat.status == SICK).map(seat => seat.pos)
  val updates =
    sick
      .map(seat => getAdjacent(seat, kernel, initial))
      .map(adjacent => getUpdate(adjacent))

}
