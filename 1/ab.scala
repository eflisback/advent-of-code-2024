@main def run = {
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))
  println(b(lines))
}

def parseColumns(lines: Iterable[String]): (Vector[Int], Vector[Int]) = {
  (for line <- lines yield
    val numbers = line.split("   ").map(_.toInt)
    (numbers(0), numbers(1))
  ).toVector.unzip
}

def a(lines: Iterable[String]): Int = {
  val (leftSorted, rightSorted) = parseColumns(lines) match
    case (left, right) => (left.sorted, right.sorted)

  (for i <- 0 until leftSorted.length
  yield math.abs(leftSorted(i) - rightSorted(i))).sum
}

def b(lines: Iterable[String]): Int = {
  val (left, right) = parseColumns(lines)

  val frequencyMap = right.groupBy(identity).view.mapValues(_.size)
  (for number <- left yield
    number * frequencyMap.getOrElse(number, 0)).sum
}
