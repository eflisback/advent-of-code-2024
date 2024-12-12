import collection.mutable.Set as MSet

type ConsumableMap = Array[Array[Option[Coordinate]]]

case class Coordinate(x: Int, y: Int)
case class Region(
    regionChar: Char,
    area: Int,
    // A border is represented by the set of its two adjacent tiles
    borders: Set[Set[Coordinate]],
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
): Vector[(Coordinate, Char)] =
  val directions = Vector((1, 0), (-1, 0), (0, 1), (0, -1))

  directions.map((dx, dy) =>
    val newX = point.x + dx
    val newY = point.y + dy

    if (0 until map.size).contains(newX) && (0 until map(0).size).contains(newY)
    then (Coordinate(newX, newY), map(newX)(newY))
    else (Coordinate(newX, newY), '-')
  )

def extractRegion(
    point: Coordinate,
    regionChar: Char,
    map: Array[Array[Char]],
    coordinates: Set[Coordinate] = Set(),
    area: Int = 0,
    borders: Set[Set[Coordinate]] = Set()
): Region =
  if (coordinates.contains(point)) {
    return Region(regionChar, area, borders, coordinates)
  }

  var newCoordinates = coordinates + point
  var newArea = area + 1
  var newBorders = MSet.from(borders)

  val neighbors = getNeighbors(point, map)

  neighbors.foreach((nCoord, nChar) =>
    if nChar == regionChar then
      val regionResult = extractRegion(
        nCoord,
        regionChar,
        map,
        newCoordinates,
        newArea,
        newBorders.toSet
      )  
      newCoordinates = regionResult.coordinates
      newArea = regionResult.area
      newBorders = MSet.from(regionResult.borders)
    else newBorders.add(Set(point, nCoord))

  )

  // neighbors.foreach((neighborCoord, neighborChar)) =>
  //     if neighborChar == regionChar then
  //       val regionResult = extractRegion(
  //         neighborCoord,
  //         regionChar,
  //         map,
  //         newCoordinates,
  //         newArea,
  //         newBorders
  //       )
  //       newCoordinates = regionResult.coordinates
  //       newArea = regionResult.area
  //       newBorders = regionResult.borders
  //     else newBorders.add(Set(point, neighborCoord))
  // ))

  Region(regionChar, newArea, newBorders.toSet, newCoordinates)

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
    region.area * region.borders.size
  ).sum

def b(lines: Vector[String]) =
  val map = getMap(lines)
  val regions = getRegions(map)