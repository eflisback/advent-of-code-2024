@main def run = {
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))
  println(b(lines))
}

def parseColumns(lines: Iterable[String]) = {
  (for line <- lines yield
    val numbers = line.split("   ").map(_.toInt)
    (numbers(0), numbers(1))
  ).toVector.unzip
}

def a(lines: Iterable[String]) = {
  val pairs = parseColumns(lines) match
    case (left, right) => (left.sorted zip right.sorted)

  (for pair <- pairs yield math.abs(pair(0) - pair(1))).sum
}

def b(lines: Iterable[String]) = {
  val (left, right) = parseColumns(lines)

  val frequencyMap = right.groupBy(identity).view.mapValues(_.size)
  (for number <- left yield number * frequencyMap.getOrElse(number, 0)).sum
}
