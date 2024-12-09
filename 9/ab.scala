type Block = Option[Int]

@main def run =
  val input = io.Source.fromFile("input.txt").getLines.next
  println("a: " + a(input))

def getBlocks(input: String) =
  input.toCharArray.zipWithIndex
    .flatMap((c, i) =>
      if (i % 2 == 0) Array.fill(c.asDigit)((Some(i / 2)))
      else Array.fill(c.asDigit)(None)
    )
    .toArray

def getFreeIndexes(blocks: Iterable[Block]) =
  (for
    (maybeInt, i) <- blocks.zipWithIndex
    if !maybeInt.isDefined
  yield i).toArray

def printBlocks(blocks: Array[Block]) =
  for maybeInt <- blocks do print("[" + maybeInt.getOrElse('.') + "]")
  println

def getRearrangedBlocks(
    blocks: Array[Block],
    freeIndexes: Array[Int],
    debug: Boolean
): Array[Block] =
  if debug then printBlocks(blocks)
  if freeIndexes.isEmpty then 
    if debug then println
    return blocks

  val modifiedBlocks = blocks.clone()

  val lastIndex = blocks.lastIndexWhere(_.isDefined)
  val swapIndex = freeIndexes.head

  if swapIndex > lastIndex then 
    if debug then println
    return blocks

  val temp = modifiedBlocks(lastIndex)
  modifiedBlocks(lastIndex) = modifiedBlocks(swapIndex)
  modifiedBlocks(swapIndex) = temp

  getRearrangedBlocks(modifiedBlocks, freeIndexes.tail, debug)

def a(input: String, debug: Boolean = false) =
  val blocks = getBlocks(input)
  val freeIndexes = getFreeIndexes(blocks)

  if debug then 
    println("Input: " + input)
    printBlocks(blocks)
    println("Free indexes: " + freeIndexes.mkString(", "))
    println
  
  getRearrangedBlocks(blocks, freeIndexes, debug).zipWithIndex
    .map((maybeInt, i) => maybeInt.getOrElse(0).toLong * i.toLong)
    .sum
