object mapped extends App {
  object Status extends Enumeration {
    type Status = Value
    val UNOCCUPIED, UNMASKED, MASKED, SICK = Value
  }

  import Status._

  case class Position(row: Int, col: Int)

  val initial = List(
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, SICK, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, SICK, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED)
  )

  def toMap[T](list: List[List[T]]) =
    (for (i <- (0 until list.length); j <- (0 until list(0).length))
      yield Position(i, j)).zip(list.flatten).toMap
  def getKernel(size: Int) = {
    val validSize = if (size < 3) 3 else if (size % 2 == 0) size + 1 else size
    for (
      (row, i) <- (1 to validSize).zipWithIndex;
      (cell, j) <- (1 to validSize).zipWithIndex
    ) yield ((i - (validSize / 2), j - validSize / 2))
  }
  def getAdjacent(
      pos: Position,
      kernel: IndexedSeq[(Int, Int)],
      seatMap: Map[Position, Status]
  ): IndexedSeq[(Position, Status)] = kernel
    .map(k => Position(pos.row - k._1, pos.col - k._2))
    .map(pos => (pos, seatMap.getOrElse(pos, UNOCCUPIED)))
    .filter(_._2 != UNOCCUPIED)

  def getUpdate(
      adjacent: IndexedSeq[(Position, Status)]
  ): IndexedSeq[(Position, Status)] = {
    adjacent.map(pos => {
      pos._2 match {
        case SICK       => (pos._1, SICK)
        case UNMASKED   => (pos._1, SICK)
        case MASKED     => (pos._1, UNMASKED)
        case UNOCCUPIED => (pos._1, UNOCCUPIED)
      }
    })
  }
  def merge(
      seatMap: Map[Position, Status],
      updates: IndexedSeq[(Position, Status)]
  ) = {
    val prev = map.get(up)
  }

  val gridMap = toMap(initial)
  val updates = gridMap
    .filter(_._2 == SICK)
    .map(cell => getAdjacent(cell._1, getKernel(3), gridMap))
    .map(cell => getUpdate(cell))
  updates.foldLeft(gridMap)((map, value) => (map, value))
}
