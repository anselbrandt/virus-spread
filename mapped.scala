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

  val flatWithIndices =
    initial
      .map(_.zipWithIndex)
      .zipWithIndex
      .map { case (row, i) =>
        row.map { case (value, j) => ((i, j), value) }
      }
      .flatten
  flatWithIndices.foreach(println)
}
