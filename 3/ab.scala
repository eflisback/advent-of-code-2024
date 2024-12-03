import scala.util.matching.Regex

@main def run = {
  val text = io.Source.fromFile("input.txt").mkString

  println(a(text))
  println(b(text))
}

def getMulMatches(text: String) = {
  val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

  pattern
    .findAllMatchIn(text)
    .map(m => (m.group(1).toInt, m.group(2).toInt))
    .toList
}

def a(text: String) = (for m <- getMulMatches(text) yield m(0) * m(1)).sum

def b(text: String) = {
  val segments = text.split("don't()")

  val initialMatches = getMulMatches(segments(0))

  val matches = (for
    i <- 1 until segments.length
    segment = segments(i)
    if segment.contains("do()")
  yield getMulMatches(segment.substring(segment.indexOf("do()") + 4))).flatten

  (for m <- initialMatches ++ matches yield m(0) * m(1)).sum
}
