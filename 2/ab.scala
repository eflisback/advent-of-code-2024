@main def run = {
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))
  println(b(lines))
}

def getLevelsMatrix(ls: Iterable[String]) = for l <- ls yield l.split(" ").map(_.toInt)

def isRowSafe(row: Array[Int]) = {
  val doesIncrease = row(1) - row(0) > 0

  (1 until row.length).forall(i =>
    val diff = row(i) - row(i - 1)
    val diffSize = math.abs(diff)
    diff > 0 == doesIncrease && (1 to 3).contains(diffSize)
  )
}

def a(lines: Iterable[String]) = getLevelsMatrix(lines).count(isRowSafe)

// O(n) time complexity streak broken :P
def b(lines: Iterable[String]) = {
  getLevelsMatrix(lines).map(_.toList).count(row =>
    (0 until row.length).exists(i =>
      isRowSafe(row.patch(i, Nil, 1).toArray))  
  )
}
