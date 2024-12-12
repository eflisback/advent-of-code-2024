type ConsumableMap = Array[Array[Option[Coordinate]]]

case class Coordinate(x: Int, y: Int)
case class Region(
    regionChar: Char,
    area: Int,
    perimeter: Int,
    coordinates: Set[Coordinate]
)

@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println("a: " + a(lines))

def getMap(lines: Vector[String]) =
  (for line <- lines yield line.toCharArray).toArray.transpose

def getNeighbors(
    point: Coordinate,
    map: Array[Array[Char]]
): Vector[Option[(Coordinate, Char)]] =
  val directions = Vector((1, 0), (-1, 0), (0, 1), (0, -1))

  directions.map((dx, dy) =>
    val newX = point.x + dx
    val newY = point.y + dy

    if (0 until map.size).contains(newX) && (0 until map(0).size).contains(newY)
    then Some((Coordinate(newX, newY), map(newX)(newY)))
    else None
  )

def extractRegion(
    point: Coordinate,
    regionChar: Char,
    map: Array[Array[Char]],
    coordinates: Set[Coordinate] = Set(),
    area: Int = 0,
    perimeter: Int = 0
): Region =
  if (coordinates.contains(point)) {
    return Region(regionChar, area, perimeter, coordinates)
  }

  var newCoordinates = coordinates + point
  var newArea = area + 1
  var newPerimeter = perimeter

  val neighbors = getNeighbors(point, map)

  neighbors.foreach {
    case Some((neighborCoord, neighborChar)) =>
      if neighborChar == regionChar then
        val regionResult = extractRegion(
          neighborCoord,
          regionChar,
          map,
          newCoordinates,
          newArea,
          newPerimeter
        )
        newCoordinates = regionResult.coordinates
        newArea = regionResult.area
        newPerimeter = regionResult.perimeter
      else newPerimeter += 1
    case None =>
      newPerimeter += 1
  }

  Region(regionChar, newArea, newPerimeter, newCoordinates)

def updateConsumableMap(
    consumableMap: Array[Array[Option[Coordinate]]],
    coordinates: Set[Coordinate]
) =
  for
    coordinate <- coordinates
    x = coordinate.x
    y = coordinate.y
  do consumableMap(x)(y) = None

def getRegions(map: Array[Array[Char]]) =
  val consumableMap: ConsumableMap =
    (for x <- map.indices
    yield (for y <- map(x).indices
    yield Some(Coordinate(x, y)): Option[Coordinate]).toArray).toArray

  for
    x <- consumableMap.indices
    y <- consumableMap(x).indices
    if consumableMap(x)(y).isDefined
  yield
    val regionChar = map(x)(y)
    val region = extractRegion(Coordinate(x, y), regionChar, map)
    updateConsumableMap(consumableMap, region.coordinates)
    region

def a(lines: Vector[String]) =
  val map = getMap(lines)
  val regions = getRegions(map)

  (for region <- regions yield
    // println("Region " + region.regionChar)
    // println(" - Area = " + region.area)
    // println(" - Perimeter = " + region.perimeter)
    region.area * region.perimeter
  ).sum
