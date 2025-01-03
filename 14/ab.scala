case class Coordinate(x: Int, y: Int)
case class Velocity(dx: Int, dy: Int)
case class Robot(position: Coordinate, velocity: Velocity)

@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println("a: " + a(lines))
  println("b: " + b(lines))

def getRobots(lines: Vector[String]) =
  for line <- lines yield
    val pos = line
      .substring(line.indexOf("p=") + 2, line.indexOf(' '))
      .split(',')
      .map(_.toInt)
    val (posX, posY) = (pos(0), pos(1))

    val vel = line.substring(line.indexOf("v=") + 2).split(',').map(_.toInt)
    val (velX, velY) = (vel(0), vel(1))

    Robot(Coordinate(posX, posY), Velocity(velX, velY))

def getFinalPositions(
    robots: Vector[Robot],
    mapDimensions: (Int, Int),
    seconds: Int
) =
  for robot <- robots
  yield
    var newX =
      (robot.position.x + seconds * robot.velocity.dx) % mapDimensions._1
    var newY =
      (robot.position.y + seconds * robot.velocity.dy) % mapDimensions._2
    if newX < 0 then newX += mapDimensions._1
    if newY < 0 then newY += mapDimensions._2
    Coordinate(newX, newY)

def a(lines: Vector[String]) =
  val robots = getRobots(lines)
  val mapDimensions = (101, 103)
  val finalPositions = getFinalPositions(robots, mapDimensions, 100)

  val center =
    (
      ((mapDimensions._1 - 1).toDouble / 2),
      ((mapDimensions._2 - 1).toDouble / 2)
    )

  var q1, q2, q3, q4 = 0
  for position <- finalPositions do
    if position.x < center._1 && position.y < center._2 then q1 += 1
    if position.x > center._1 && position.y < center._2 then q2 += 1
    if position.x < center._1 && position.y > center._2 then q3 += 1
    if position.x > center._1 && position.y > center._2 then q4 += 1

  q1 * q2 * q3 * q4

def getChristmasTrees(mapDimensions: (Int, Int)) =
  import collection.mutable.Set as MSet
  val christmasTrees: MSet[Set[Coordinate]] = MSet()

  val centerX = (mapDimensions._1 - 1).toDouble / 2

  for y <- 0 until mapDimensions._2 do
    val christmasTree: MSet[Coordinate] = MSet()

    for yy <- y until mapDimensions._2
    do
        val left = Coordinate(x = (centerX - yy).toInt, yy)
        val right = Coordinate(x = (centerX + yy).toInt, yy)
        if left.x >= 0 then
            christmasTree.add(left)
            christmasTree.add(right)

        christmasTrees.add(christmasTree.toSet)

  christmasTrees.toSet

def b(lines: Vector[String]) =
  val robots = getRobots(lines)
  val mapDimensions = (101, 103)

  val christmasTrees: Set[Set[Coordinate]] = getChristmasTrees(mapDimensions)

  (for
    i <- 1 to 10000
    if christmasTrees.exists(christmasTree => christmasTree.subsetOf(getFinalPositions(robots, mapDimensions, i).toSet) && christmasTree.size > robots.size / 2)
  yield i).min
