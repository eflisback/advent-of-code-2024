case class Coordinate(x: Int, y: Int)
type TopographyMap = Array[Array[Int]]

@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println("a: " + a(lines))
  println("b: " + b(lines))

def getTopographyMap(lines: Iterable[String]) =
  (for line <- lines
  yield line.toCharArray.map(_.asDigit)).toArray.transpose

def getHeight(point: Coordinate, map: TopographyMap) =
  if (0 until map.size).contains(point.x) && (0 until map(0).size)
      .contains(point.y)
  then Some(map(point.x)(point.y))
  else None

def getReachablePeaks(
    point: Coordinate,
    map: TopographyMap,
    peaks: Set[Coordinate] = Set()
): Set[Coordinate] =
  val height = map(point.x)(point.y)

  if height == 9 then return peaks + point

  val directions = Set((1, 0), (-1, 0), (0, 1), (0, -1))
  if !directions.exists(d =>
      val nextHeight =
        getHeight(Coordinate(point.x + d._1, point.y + d._2), map)
      nextHeight.isDefined && nextHeight.get == height + 1
    )
  then return Set()

  directions.foldLeft(peaks)((accumulatedPeaks, direction) =>
    val nextPoint = Coordinate(point.x + direction._1, point.y + direction._2)
    val nextHeight = getHeight(nextPoint, map)

    if nextHeight.isDefined && nextHeight.get == height + 1 then
      accumulatedPeaks ++ getReachablePeaks(nextPoint, map, peaks)
    else accumulatedPeaks
  )

def a(lines: Iterable[String]) =
  val map = getTopographyMap(lines)

  (for
    x <- map.indices
    y <- map(x).indices
    if map(x)(y) == 0
  yield getReachablePeaks(Coordinate(x, y), map).size).sum

def getUniqueTrails(
    point: Coordinate,
    map: TopographyMap,
    trails: Set[Set[Coordinate]] = Set(),
    currentTrail: Set[Coordinate] = Set()
): Set[Set[Coordinate]] = 
  val height = map(point.x)(point.y)

  if height == 9 then return Set(currentTrail + point)

  val directions = Set((1, 0), (-1, 0), (0, 1), (0, -1))
  if !directions.exists(d =>
      val nextHeight =
        getHeight(Coordinate(point.x + d._1, point.y + d._2), map)
      nextHeight.isDefined && nextHeight.get == height + 1
    )
  then return Set()

  directions.foldLeft(trails)((accumulatedTrails, direction) =>
    val nextPoint = Coordinate(point.x + direction._1, point.y + direction._2)
    val nextHeight = getHeight(nextPoint, map)

    if nextHeight.isDefined && nextHeight.get == height + 1 then
      accumulatedTrails ++ getUniqueTrails(nextPoint, map, trails, currentTrail + point)
    else accumulatedTrails
  )

def b(lines: Iterable[String]) =
  val map = getTopographyMap(lines)

  (for
    x <- map.indices
    y <- map(x).indices
    if map(x)(y) == 0
  yield getUniqueTrails(Coordinate(x, y), map).size).sum
