@main def run =
  val input = io.Source.fromFile("input.txt").getLines.toVector.head

  println("a: " + a(input))

// Vector type for fast insertion
def getStones(input: String) =
  (for num <- input.split(" ") yield num.toLong).toVector

def getMutatedStones(stones: Vector[Long]) = 
    var i = 0
    var mutation = stones

    while i < mutation.size do
        val stone = mutation(i)
        val str = stone.toString

        if stone == 0 then
            mutation = mutation.updated(i, 1)
        else if str.length % 2 == 0 then
            val (leftStr, rightStr) = str.splitAt(str.length / 2)
            val left = leftStr.toLong
            val right = rightStr.toLong
            mutation = mutation.updated(i, left).patch(i + 1, Seq(right), 0)
            i += 1
        else
            mutation = mutation.updated(i, stone * 2024)
        
        i += 1
      
    mutation


def getFinalStoneArrangement(stones: Vector[Long], remainingBlinks: Int): Vector[Long] =
    if remainingBlinks == 0 then return stones

    val mutatedStones = getMutatedStones(stones)

    getFinalStoneArrangement(mutatedStones, remainingBlinks - 1)

def a(input: String) =
  val stones = getStones(input)

  getFinalStoneArrangement(stones, 25).size
