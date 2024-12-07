@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))
  println(b(lines))

def getEquationsData(lines: Vector[String]) =
  for line <- lines yield
    val split = line.split(':')
    (split(0).toLong, split(1).split(' ').filterNot(_.isBlank).map(_.toLong))

def couldWork(
    equation: (Long, Array[Long]),
    value: Long = 0,
    index: Int = 0
): Boolean = {
  val targetValue = equation._1
  val operands = equation._2

  if index == operands.length then return value == targetValue

  val nextOperand = operands(index)
  couldWork(equation, value + nextOperand, index + 1) ||
  couldWork(equation, value * nextOperand, index + 1)
}

def couldWorkWithConcat(
    equation: (Long, Array[Long]),
    value: Long = 0,
    index: Int = 0
): Boolean = {
  val targetValue = equation._1
  val operands = equation._2

  if index == operands.length then return value == targetValue

  val nextOperand = operands(index)
  couldWorkWithConcat(equation, value + nextOperand, index + 1) ||
  couldWorkWithConcat(equation, value * nextOperand, index + 1) ||
  couldWorkWithConcat(equation, (value.toString + nextOperand.toString).toLong, index + 1)
}

def a(lines: Vector[String]) =
  (for
    equation <- getEquationsData(lines)
    if couldWork(equation)
  yield equation._1).sum

def b(lines: Vector[String]) =
  (for
    equation <- getEquationsData(lines)
    if couldWorkWithConcat(equation)
  yield equation._1).sum