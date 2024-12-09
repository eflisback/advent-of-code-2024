@main def run =
  val input = io.Source.fromFile("input.txt").getLines.next
  println("Input: " + input)

  println("Sum: " + a(input))

def getBlocks(input: String) = input.toCharArray.zipWithIndex
  .flatMap((c, i) =>
    if (i % 2 == 0) Array.fill(c.asDigit)((i / 2).toString.head)
    else Array.fill(c.asDigit)('.')
  )
  .toArray

def getFreeIndexes(blocks: Iterable[Char]) =
  (for
    (char, i) <- blocks.zipWithIndex
    if char == '.'
  yield i).toArray

def getRearrangedBlocks(
    blocks: Array[Char],
    freeIndexes: Array[Int]
): Array[Char] =
  if freeIndexes.isEmpty then 
    println("Rearranged blocks: " + blocks.mkString)
    return blocks

  val modifiedBlocks = blocks.clone()

  val lastIndex = blocks.lastIndexWhere(_ != '.')
  val swapIndex = freeIndexes.head

  if swapIndex > lastIndex then 
    println("Rearranged blocks: " + blocks.mkString)
    return blocks

  val temp = modifiedBlocks(lastIndex)
  modifiedBlocks(lastIndex) = modifiedBlocks(swapIndex)
  modifiedBlocks(swapIndex) = temp

  getRearrangedBlocks(modifiedBlocks, freeIndexes.tail)

def a(input: String) =
  val blocks = getBlocks(input)
  println("Blocks: " + blocks.mkString)
  val freeIndexes = getFreeIndexes(blocks)
  getRearrangedBlocks(blocks, freeIndexes).zipWithIndex
    .map((c, i) => if c != '.' then c.asDigit * i else 0)
    .sum
