type Coordinate = (Int, Int)
val directionChars = Array('<', '^', '>', 'v')

@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))

def getMap(lines: Vector[String]) =
  for line <- lines yield line.toCharArray

def getStartPosition(
    lines: Vector[String]
): Coordinate =
  lines.zipWithIndex
    .flatMap((line, y) =>
      line.zipWithIndex.collect {
        case (char, x) if directionChars.contains(char) => (x, y)
      }
    )
    .head

def isInBounds(position: Coordinate, map: Vector[Array[Char]]) =
  (0 until map.size).contains(position._2) && (0 until map(0).size)
    .contains(position._1)

def getDirectionTuple(directionSign: Char) =
  directionSign match
    case '<'     => (-1, 0)
    case '^'     => (0, -1)
    case '>'     => (1, 0)
    case 'v'     => (0, 1)
    case _: Char => (0, 0)

def a(lines: Vector[String]) =
  val map = getMap(lines)

  // Immutability streak broken :P
  var position: Coordinate = getStartPosition(lines)
  var direction: (Int, Int) = getDirectionTuple(map(position._2)(position._1))
  val visitedPositions: collection.mutable.Set[Coordinate] =
    collection.mutable.Set()

  while isInBounds(position, map) do
    visitedPositions += position
    val forwardPosition =
      (position._1 + direction._1, position._2 + direction._2)
    
    if !isInBounds(forwardPosition, map) then
      position = forwardPosition
    else
      val forwardChar = map(forwardPosition._2)(forwardPosition._1)
      if forwardChar == '#' then
        direction = (-direction._2, direction._1)
      else
        position = forwardPosition

  visitedPositions.size
