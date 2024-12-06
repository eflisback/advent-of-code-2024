type Coordinate = (Int, Int)

@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))
  println(b(lines))

def getMap(lines: Vector[String]) =
  for line <- lines yield line.toCharArray

def printMap(map: Vector[Array[Char]]) = for row <- map do println(row.mkString)

def getStartPosition(map: Vector[Array[Char]]): Coordinate =
  map.indices
    .flatMap { y =>
      map(y).indices.collect {
        case x if Set('<', '^', '>', 'v').contains(map(y)(x)) => (x, y)
      }
    }
    .headOption
    .getOrElse(
      throw new NoSuchElementException("No start position found in the map")
    )

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

def getForwardPosition(position: Coordinate, direction: (Int, Int)) =
  (position._1 + direction._1, position._2 + direction._2)

def rotated(direction: (Int, Int)) = (-direction._2, direction._1)

def getNewPositionAndDirection(
    position: Coordinate,
    direction: (Int, Int),
    map: Vector[Array[Char]]
): (Coordinate, (Int, Int)) =
  val forwardPosition = getForwardPosition(position, direction)

  if !isInBounds(forwardPosition, map) then (forwardPosition, direction)
  else
    val forwardChar = map(forwardPosition._2)(forwardPosition._1)
    if forwardChar == '#' then (position, rotated(direction))
    else (forwardPosition, direction)

def getPathData(
    map: Vector[Array[Char]],
    startPosition: Coordinate,
    startDirection: (Int, Int)
) = {
  var position = startPosition
  var direction = startDirection

  val pathData: collection.mutable.Set[(Coordinate, (Int, Int))] =
    collection.mutable.Set()

  while isInBounds(position, map) do
    pathData += ((position, direction))
    val (newPosition, newDirection) =
      getNewPositionAndDirection(position, direction, map)
    position = newPosition
    direction = newDirection

  pathData.toSet
}

def a(lines: Vector[String]) =
  val map = getMap(lines)
  val startPosition = getStartPosition(map)

  getPathData(
    map,
    startPosition,
    getDirectionTuple(map(startPosition._2)(startPosition._1))
  ).groupBy((c, p) => c).size

def getMapWithObstacleAt(
    map: Vector[Array[Char]],
    position: Coordinate
) =
  (for y <- map.indices
  yield (for x <- map(0).indices
  yield if position == (x, y) then '#' else map(y)(x)).toArray).toVector

def resultsInLoop(mutatedMap: Vector[Array[Char]]): Boolean =
  var position = getStartPosition(mutatedMap)
  var direction = getDirectionTuple(mutatedMap(position._2)(position._1))

  val pathData: collection.mutable.Set[(Coordinate, (Int, Int))] =
    collection.mutable.Set()

  while isInBounds(position, mutatedMap) do
    val dataPoint = ((position, direction))
    if pathData.contains(dataPoint) then return true
    pathData += ((position, direction))
    val (newPosition, newDirection) =
      getNewPositionAndDirection(position, direction, mutatedMap)
    position = newPosition
    direction = newDirection

  return false

def b(lines: Vector[String]) = {
  val map = getMap(lines)
  val startPosition = getStartPosition(map)

  val potentialObstaclePositions = getPathData(
    map,
    startPosition,
    getDirectionTuple(map(startPosition._2)(startPosition._1))
  ).map((c, _) => c).filterNot(c => c == startPosition)
  
  (for
    position <- potentialObstaclePositions
    if resultsInLoop(getMapWithObstacleAt(map, position))
  yield position).size
}
