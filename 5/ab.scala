@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector
  val (ruleLines, remainingLines) = lines.span(_.trim.nonEmpty)
  val updateLines = remainingLines.drop(1)

  println(a(ruleLines, updateLines))
  println(b(ruleLines, updateLines))

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
  if update.isEmpty then return true

  val current = update.head
  val remainingUpdate = update.tail

  if remainingUpdate.exists(next =>
      ruleMap.get(next).exists(_.contains(current))
    )
  then return false

  isValid(remainingUpdate, ruleMap.filterKeys(_ != current).toMap)

def a(ruleLines: Vector[String], updateLines: Vector[String]) =
  val ruleMap = getRuleMap(ruleLines)
  val updates = getUpdates(updateLines)

  (for
    update <- updates
    if isValid(update, ruleMap)
  yield update(math.floor(update.size / 2).toInt)).sum

def getCorrectedUpdate(
    update: Array[Int],
    ruleMap: Map[Int, Set[Int]]
) =
  val filteredKeys = ruleMap.filterKeys(k => update.contains(k))
  val filteredValues =
    filteredKeys.map((key, valueSet) => key -> valueSet.filter(update.contains))

  val positionMap =
    filteredValues.map((key, valueSet) => key -> valueSet.size).toMap

  update.sortBy(value => positionMap.getOrElse(value, 0))

def b(ruleLines: Vector[String], updateLines: Vector[String]) =
  val ruleMap = getRuleMap(ruleLines)
  val updates = getUpdates(updateLines)

  (for
    update <- updates
    if !isValid(update, ruleMap)
  yield getCorrectedUpdate(update, ruleMap)(
    math.floor(update.size / 2).toInt
  )).sum
