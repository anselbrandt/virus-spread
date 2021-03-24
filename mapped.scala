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
  def getAdjacent[T](
      pos: Position,
      kernel: IndexedSeq[(Int, Int)],
      seatMap: Map[Position, Status]
  ) = kernel
    .map(k => Position(pos.row - k._1, pos.col - k._2))
    .map(pos => (pos, seatMap.get(pos)))
    .filter(_._2.isDefined)
  // .map { case (pos, Some(status)) => (pos, status) }

  val seatMap = toMap(initial)
  val kernel = getKernel(3)
  val adjacent = getAdjacent(Position(0, 0), kernel, seatMap)
  val sick = seatMap.filter(_._2 == SICK)
  println(adjacent)
}
