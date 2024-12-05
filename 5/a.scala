@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector
  val (ruleLines, remainingLines) = lines.span(_.trim.nonEmpty)
  val updateLines = remainingLines.drop(1)

  println(a(ruleLines, updateLines))

def getRuleMap(ruleLines: Vector[String]) =
  ruleLines
    .map(line =>
      val Array(key, value) = line.split('|').map(_.trim)
      key.toInt -> value.toInt
    )
    .groupMap(_._1)(_._2)
    .view
    .mapValues(_.toSet)
    .toMap

def getUpdates(updateLines: Vector[String]) =
  for line <- updateLines yield line.split(',').map(_.toInt)

def isValid(update: Array[Int], ruleMap: Map[Int, Set[Int]]): Boolean =
    val relevantRules = ruleMap.filter((k, _) => update.contains(k))

    
´

def a(ruleLines: Vector[String], updateLines: Vector[String]) =
  val ruleMap = getRuleMap(ruleLines)
  val updates = getUpdates(updateLines)

  (for
    update <- updates
    if isValid(update, ruleMap)
  yield update(math.floor(update.size / 2).toInt)).sum
