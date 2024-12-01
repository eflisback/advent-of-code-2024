@main def run = {

  val lines = io.Source.fromFile("input.txt").getLines

  val (leftValues, rightValues) =
    (for line <- lines yield
      val values = line.split("   ").map(_.toInt)
      (values(0), values(1))
    ).toVector.unzip

  val sortedLeftValues = leftValues.sorted
  val sortedRightValues = rightValues.sorted

  val sum =
    (for i <- 0 until sortedLeftValues.length
    yield math.abs(sortedLeftValues(i) - sortedRightValues(i))).sum

  println(sum)

}
