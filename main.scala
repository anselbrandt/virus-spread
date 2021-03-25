object main extends App {
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

  def toMap[T](list: List[List[T]]): Map[Position, T] =
    (for (i <- (0 until list.length); j <- (0 until list(0).length))
      yield Position(i, j)).zip(list.flatten).toMap

  def getKernel(size: Int): IndexedSeq[(Int, Int)] = {
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

  def getUpdates(gridMap: Map[Position, Value]): Iterable[(Position, Status)] =
    gridMap
      .filter(_._2 == SICK)
      .map(cell => getAdjacent(cell._1, getKernel(3), gridMap))
      .map(cell => getUpdate(cell))
      .flatten

  def recursive(
      gridMap: Map[Position, Value],
      count: Int = 0
  ): (Map[Position, Value], Int) = {
    val prev = gridMap
    val curr = getUpdates(gridMap).foldLeft(
      gridMap
    )((map, update) => {
      (map.get(update._1), update._2) match {
        case (UNMASKED, SICK)   => map + (update._1 -> SICK)
        case (MASKED, UNMASKED) => map + (update._1 -> UNMASKED)
        case (_, _)             => map + (update._1 -> update._2)
      }
    })
    if (prev == curr) (curr, count)
    else (recursive(curr, count + 1)._1, recursive(curr, count + 1)._2)
  }

  val gridMap = toMap(initial)
  val spread = recursive(gridMap)
  println(spread._2)
}
