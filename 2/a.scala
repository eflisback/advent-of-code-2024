@main def run = {
    val lines = io.Source.fromFile("input.txt").getLines.toVector
    
    println(a(lines))
}

def getLevelsMatrix(lines: Iterable[String]) = {
    for line <- lines yield
        line.split(" ").map(_.toInt)
}

def isRowSafe(row: Array[Int]) = {
    val doesIncrease = row(1) - row(0) > 0

    (1 until row.length).forall(i =>
        val diff = row(i) - row(i - 1)
        val diffSize = math.abs(diff)
        diff > 0 == doesIncrease && (1 to 3).contains(diffSize)
    )
}

def a(lines: Iterable[String]) = {
    val levelsMatrix = getLevelsMatrix(lines)

    (for row <- levelsMatrix yield
        if isRowSafe(row) then 1 else 0).sum
}