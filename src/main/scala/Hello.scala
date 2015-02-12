import chapitre4.Stream2

object Hello {
  def main(args: Array[String]): Unit = {
    println(Stream2(1,2,3).take(5))//.toListRecursive)
  }
}
