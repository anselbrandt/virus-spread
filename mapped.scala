object mapped extends App {
  object Status extends Enumeration {
    type Status = Value
    val UNOCCUPIED, UNMASKED, MASKED, SICK = Value
  }

  import Status._

  case class Position(row: Int, col: Int)

  val initial = List(
    List(MASKED, SICK, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, MASKED, MASKED),
    List(MASKED, MASKED, MASKED, SICK, MASKED)
  )

  def flatWithIndices[T](list2d: List[List[T]]) =
    list2d
      .map(_.zipWithIndex)
      .zipWithIndex
      .map { case (row, i) =>
        row.map { case (value, j) => ((i, j), value) }
      }
      .flatten
  val flattened = flatWithIndices(initial)
  flattened.foreach(println)
}
