type Coordinate = (Int, Int)
type CharMatrix = Array[Array[Char]]
type Word = Array[Coordinate]

@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector
  val matrix: CharMatrix = parseCharMatrix(lines)

  println(a(matrix))
  println(b(matrix))

def parseCharMatrix(ls: Iterable[String]) =
  (for l <- ls yield l.toCharArray).toArray

def isInBounds(coord: Coordinate, matrix: CharMatrix) =
  (0 until matrix.size).contains(coord._1) && (0 until matrix(0).size)
    .contains(coord._2)

def getNeighbors(coord: Coordinate) =
  val (x, y) = coord
  for
    ax <- (x - 1) to (x + 1)
    ay <- (y - 1) to (y + 1)
    if !(ax == x && ay == y)
  yield (ax, ay)

def getDirection(from: Coordinate, to: Coordinate): Coordinate =
  (to._1 - from._1, to._2 - from._2)

def getWords(
    coord: Coordinate,
    matrix: CharMatrix,
    visited: List[Coordinate] = List()
): Set[Option[Word]] =
  val word = "XMAS"
  val targetIndex = visited.size

  if targetIndex == word.size then return Set(Some(visited.toArray))
  if !isInBounds(coord, matrix) then return Set(None)

  val currentChar = matrix(coord._1)(coord._2)
  if currentChar != word(targetIndex) then return Set(None)

  val newVisited = coord :: visited

  if visited.isEmpty then
    getNeighbors(coord)
      .flatMap(neighbor => getWords(neighbor, matrix, newVisited))
      .toSet
  else
    val last = coord
    val secondLast = visited.head
    val direction = getDirection(secondLast, last)
    val nextCoord = (coord._1 + direction._1, coord._2 + direction._2)
    getWords(nextCoord, matrix, newVisited)

def a(matrix: CharMatrix) =
  (for
    x <- matrix.indices
    y <- matrix(x).indices
  yield getWords((x, y), matrix)).flatten.count(_.isDefined)

def isX(coord: Coordinate, matrix: CharMatrix): Boolean =
  val word = "MAS"

  val firstList =
    List((coord._1 - 1, coord._2 - 1), coord, (coord._1 + 1, coord._2 + 1))
  val firstWord = (for c <- firstList yield matrix(c._1)(c._2)).mkString

  val secondList =
    List((coord._1 + 1, coord._2 - 1), coord, (coord._1 - 1, coord._2 + 1))
  val secondWord = (for c <- secondList yield matrix(c._1)(c._2)).mkString

  (firstWord == word || firstWord.reverse == word) && (secondWord == word || secondWord.reverse == word)

def b(matrix: CharMatrix) =
  (for
    x <- 1 until matrix.size - 1
    y <- 1 until matrix(x).size - 1
    if matrix(x)(y) == 'A'
  yield isX((x, y), matrix)).count(_ == true)