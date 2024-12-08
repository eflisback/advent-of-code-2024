import collection.mutable.Set as MSet
import collection.mutable.Map as MMap

type AntennaMap = Vector[Array[Char]]
type Coordinate = (Int, Int)

@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))
  println(b(lines))

def getMap(lines: Vector[String]): AntennaMap =
  lines.map(_.toCharArray).transpose.map(_.toArray)

def getfrequencies(map: AntennaMap) =
  val frequencies: MMap[Char, MSet[Coordinate]] = MMap()

  for
    x <- map.indices
    y <- map(0).indices
  do
    val char = map(x)(y)
    if char != '.' then
      if frequencies.keySet.contains(char) then
        frequencies.get(char).get += ((x, y))
      else frequencies.put(char, MSet((x, y)))

  frequencies.view.mapValues(s => s.toSet).toMap

def getAntinodesAround(a: Coordinate, b: Coordinate) =
  val distanceX = b._1 - a._1
  val distanceY = b._2 - a._2

  Set(
    (a._1 - distanceX, a._2 - distanceY),
    (b._1 + distanceX, b._2 + distanceY)
  )

def isInBounds(point: Coordinate, map: Vector[Array[Char]]) =
  (0 until map.size).contains(point._1) &&
    (0 until map(0).size).contains(point._2)

def a(lines: Vector[String]) =
  val map = getMap(lines)
  val frequencies = getfrequencies(map)

  (for
    antennas <- frequencies.map(_._2)
    firstAntenna <- antennas
    otherAntenna <- antennas - firstAntenna
  yield getAntinodesAround(firstAntenna, otherAntenna).filterNot(p =>
    p == firstAntenna || p == otherAntenna
  )).flatten.toSet
    .filter(point => isInBounds(point, map))
    .size

def getAntinodesAlongLine(
    a: Coordinate,
    b: Coordinate,
    map: Vector[Array[Char]]
): Set[Coordinate] =
  val distanceX = b._1 - a._1
  val distanceY = b._2 - a._2

  val backwardsLocations: MSet[Coordinate] = MSet(a)
  val forwardsLocations: MSet[Coordinate] = MSet(b)

  var currentPoint = a

  while isInBounds(currentPoint, map) do
    val point = (currentPoint._1 - distanceX, currentPoint._2 - distanceY)
    backwardsLocations += point
    currentPoint = point

  currentPoint = b

  while isInBounds(currentPoint, map) do
    val point = (currentPoint._1 + distanceX, currentPoint._2 + distanceY)
    forwardsLocations += point
    currentPoint = point

  backwardsLocations.toSet ++ forwardsLocations.toSet

def b(lines: Vector[String]) =
  val map = getMap(lines)
  val frequencies = getfrequencies(map)

  (for
    antennas <- frequencies.map(_._2)
    firstAntenna <- antennas
    otherAntenna <- antennas - firstAntenna
  yield getAntinodesAlongLine(firstAntenna, otherAntenna, map)).flatten.toSet
    .filter(point => isInBounds(point, map))
    .size
