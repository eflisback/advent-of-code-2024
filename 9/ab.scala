@main def run =
  val input = io.Source.fromFile("input.txt").getLines.next
  println("Input: " + input)

  println("Sum: " + a(input))

def getBlocks(input: String): Array[Option[Int]] =
  input.toCharArray.zipWithIndex
    .flatMap((c, i) =>
      if (i % 2 == 0) Array.fill(c.asDigit)((Some(i / 2)))
      else Array.fill(c.asDigit)(None)
    )
    .toArray

def getFreeIndexes(blocks: Iterable[Option[Int]]) =
  (for
    (maybeInt, i) <- blocks.zipWithIndex
    if !maybeInt.isDefined
  yield i).toArray

def printBlocks(blocks: Array[Option[Int]]) =
  for maybeInt <- blocks do
    if maybeInt.isDefined then print("[" + maybeInt.get + "]") else print('.')
  println()

def getRearrangedBlocks(
    blocks: Array[Option[Int]],
    freeIndexes: Array[Int]
): Array[Option[Int]] =
  printBlocks(blocks)
  if freeIndexes.isEmpty then return blocks

  val modifiedBlocks = blocks.clone()

  val lastIndex = blocks.lastIndexWhere(_.isDefined)
  val swapIndex = freeIndexes.head

  if swapIndex > lastIndex then return blocks

  val temp = modifiedBlocks(lastIndex)
  modifiedBlocks(lastIndex) = modifiedBlocks(swapIndex)
  modifiedBlocks(swapIndex) = temp

  getRearrangedBlocks(modifiedBlocks, freeIndexes.tail)

def a(input: String) =
  val blocks = getBlocks(input)
  printBlocks(blocks)
  val freeIndexes = getFreeIndexes(blocks)
  println("Free indexes: " + freeIndexes.mkString(", "))
  println("----------")
  getRearrangedBlocks(blocks, freeIndexes).zipWithIndex
    .map((maybeInt, i) => if maybeInt.isDefined then maybeInt.get * i else 0)
    .sum
