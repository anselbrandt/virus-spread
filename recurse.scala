object recurse extends App {
  def fact(i: Int, n: Int = 0): (Int, Int) = {
    if (i == 1) (i, n) else (i * fact(i - 1, n + 1)._1, fact(i - 1, n + 1)._2)
  }
  val result = fact(16)
  println(result)
}
