type Coordinate = (Int, Int)

case class Game(a: Coordinate, b: Coordinate, target: Coordinate)

@main def run = 
    val lines = io.Source.fromFile("input.txt").getLines.toVector.appended("")

    println("a: " + a(lines))

def getGames(lines: Vector[String]) = 
    import collection.mutable.Set as MSet

    val games: MSet[Game] = MSet()

    var aButtonValue: Coordinate = (-1, -1)
    var bButtonValue: Coordinate = (-1, -1)
    var target: Coordinate = (-1, -1)

    for (line, index) <- lines.zipWithIndex do
        val rest = index % 4

        if rest == 0 then
            aButtonValue = (
                line.substring(line.indexOf('X') + 1, line.indexOf(',')).toInt,
                line.substring(line.indexOf('Y') + 1).toInt,
            )
        else if rest == 1 then
            bButtonValue = (
                line.substring(line.indexOf('X') + 1, line.indexOf(',')).toInt,
                line.substring(line.indexOf('Y') + 1).toInt,
            )
        else if rest == 2 then 
            target = (
                line.substring(line.indexOf("X=") + 2, line.indexOf(',')).toInt,
                line.substring(line.indexOf("Y=") + 2).toInt
            )
        else
            games.add(Game(aButtonValue, bButtonValue, target))
    
    games.toSet

def getCheapestPrice(game: Game, position: (Int, Int) = (0, 0)): Int =
  val (targetX, targetY) = game.target
  val (dxa, dya) = game.a
  val (dxb, dyb) = game.b

  var cheapestPrice = Int.MaxValue

  for 
    noAPresses <- 1 to 100
    noBPresses <- 1 to 100
    if noAPresses * dxa + noBPresses * dxb == targetX
    if noAPresses * dya + noBPresses * dyb == targetY
  do
    val currentPrice = noAPresses * 3 + noBPresses * 1
    cheapestPrice = math.min(cheapestPrice, currentPrice)
  
  cheapestPrice

def a(lines: Vector[String]) = 
    val games = getGames(lines)
    println(games.size)

    (for game <- games.toList yield getCheapestPrice(game)).filterNot(_ == Int.MaxValue).sum

