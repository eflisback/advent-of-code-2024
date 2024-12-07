@main def run =
  val lines = io.Source.fromFile("input.txt").getLines.toVector

  println(a(lines))
  println(b(lines))

def getEquationsData(lines: Vector[String]) =
  for line <- lines yield
    val split = line.split(':')
    (split(0).toLong, split(1).split(' ').filterNot(_.isBlank).map(_.toLong))

def hasSolution(
    equation: (Long, Array[Long]),
    allowedOperations: Seq[(Long, Long) => Long],
    value: Long = 0,
    index: Int = 0
): Boolean = {
  val targetValue = equation._1
  val operands = equation._2

  if index == operands.length then return value == targetValue

  val nextOperand = operands(index)
  allowedOperations.exists(op =>
    hasSolution(equation, allowedOperations, op(value, nextOperand), index + 1)
  )
}

val addition = (a: Long, b: Long) => a + b
val multiplication = (a: Long, b: Long) => a * b

def a(lines: Vector[String]) =
  val allowedOperations = Seq(
    addition,
    multiplication
  )

  (for
    equation <- getEquationsData(lines)
    if hasSolution(equation, allowedOperations)
  yield equation._1).sum

val concatination = (a: Long, b: Long) => (a.toString + b.toString).toLong

def b(lines: Vector[String]) =
  val allowedOperations = Seq(
    addition,
    multiplication,
    concatination
  )

  (for
    equation <- getEquationsData(lines)
    if hasSolution(equation, allowedOperations)
  yield equation._1).sum
