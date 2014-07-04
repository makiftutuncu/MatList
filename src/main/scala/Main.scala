object Main extends App {
  val list = MatList(1,2,3)
  println(list.foldLeft(0)(a))

  def a(i: Int, j: Int): Int = i + j
}
