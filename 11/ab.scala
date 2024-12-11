import scala.collection.mutable.Map as MMap

@main def run =
  val input = io.Source.fromFile("input.txt").getLines.toVector.head

  println("a: " + getMutatedStones(getStones(input), 25).values.sum)
  println("b: " + getMutatedStones(getStones(input), 75).values.sum)

def getStones(input: String) = for num <- input.split(" ") yield num.toLong

def getMutatedStones(stones: Iterable[Long], blinks: Int): Map[Long, Long] =
  // The line below, I do admit, was written by ChatGPT
  // It generates a map where each stone number is a key
  // that has the value of the number of times it occurs
  var stateCounts = stones.groupMapReduce(identity)(_ => 1L)(_ + _)

  for _ <- 1 to blinks do
    val newStateCounts = MMap[Long, Long]()

    for (stone, count) <- stateCounts do
      val str = stone.toString
      if stone == 0 then
        newStateCounts(1L) = newStateCounts.getOrElse(1L, 0L) + count
      else if str.length % 2 == 0 then
        val (leftStr, rightStr) = str.splitAt(str.length / 2)
        val left = leftStr.toLong
        val right = rightStr.toLong
        newStateCounts(left) = newStateCounts.getOrElse(left, 0L) + count
        newStateCounts(right) = newStateCounts.getOrElse(right, 0L) + count
      else
        val newStone = stone * 2024
        newStateCounts(newStone) =
          newStateCounts.getOrElse(newStone, 0L) + count

    stateCounts = newStateCounts.toMap

  stateCounts
