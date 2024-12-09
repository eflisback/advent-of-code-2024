import collection.mutable.ListBuffer

type Block = Option[Int]

@main def run =
  val input = io.Source.fromFile("input.txt").getLines.next

  println("a: " + a(input))
  println("b: " + b(input))

def getDiskSegments(input: String): Array[Vector[Block]] =
  input.toCharArray.zipWithIndex
    .map((c, i) =>
      if (i % 2 == 0) Vector.fill(c.asDigit)((Some(i / 2)))
      else Vector.fill(c.asDigit)(None)
    )
    .toArray

def getFreeIndexes(blocks: Iterable[Block]) =
  (for
    (maybeInt, i) <- blocks.zipWithIndex
    if !maybeInt.isDefined
  yield i).toArray

def printBlocks(blocks: Iterable[Block]) =
  for maybeInt <- blocks do print("[" + maybeInt.getOrElse('.') + "]")
  println

def swapElements[T](array: Array[T], index1: Int, index2: Int) =
  val temp = array(index1)
  array(index1) = array(index2)
  array(index2) = temp

def getRearrangedBlocks(
    blocks: Array[Block],
    freeIndexes: Array[Int],
    debug: Boolean
): Array[Block] =
  if debug then printBlocks(blocks)
  if freeIndexes.isEmpty then
    if debug then println
    return blocks

  val modifiedBlocks = blocks.clone

  val lastIndex = blocks.lastIndexWhere(_.isDefined)
  val swapIndex = freeIndexes.head

  if swapIndex > lastIndex then
    if debug then println
    return blocks

  swapElements(modifiedBlocks, lastIndex, swapIndex)

  getRearrangedBlocks(modifiedBlocks, freeIndexes.tail, debug)

def a(input: String, debug: Boolean = false) =
  val blocks = getDiskSegments(input).flatten
  val freeIndexes = getFreeIndexes(blocks)

  if debug then
    println("Input: " + input)
    printBlocks(blocks)
    println("Free indexes: " + freeIndexes.mkString(", "))
    println

  getRearrangedBlocks(blocks, freeIndexes, debug).zipWithIndex
    .map((maybeInt, i) => maybeInt.getOrElse(0).toLong * i.toLong)
    .sum

// Thanks, ChatGPT
def getAvailableSlot(
    freeIndexes: ListBuffer[Int],
    segmentSize: Int
): Option[Range] =
  val sortedIndexes = freeIndexes.sorted

  sortedIndexes
    .foldLeft(ListBuffer[ListBuffer[Int]]()) { (acc, index) =>
      if (acc.isEmpty || index != acc.last.last + 1) {
        acc.append(ListBuffer(index))
      } else {
        acc.last.append(index)
      }
      acc
    }
    .find(_.size >= segmentSize)
    .map(seq => seq.take(segmentSize).head to seq.take(segmentSize).last)

// Not as clean as A :P
def b(input: String, debug: Boolean = false) =

  val segments = getDiskSegments(input)
  val blocks = segments.flatten.clone
  val freeIndexes = ListBuffer.from(getFreeIndexes(blocks))

  if debug then
    println("Input: " + input)
    printBlocks(blocks)
    println("Free indexes: " + freeIndexes.mkString(", "))
    println

  for segment <- segments.reverse
  do
    val segmentSample = if segment.size > 0 then segment.head else None
    if segmentSample.isDefined then
      val segmentSize = segment.size
      val currentFileLocationIndexes =
        for
          (block, i) <- blocks.zipWithIndex
          if block.getOrElse(-1) == segmentSample.get
        yield i
      freeIndexes.filterInPlace(i => i < currentFileLocationIndexes.head)

      val availableSlot = getAvailableSlot(freeIndexes, segmentSize)
      if availableSlot.isDefined then
        val availableSlotArray = availableSlot.get.toArray

        for i <- 0 until availableSlotArray.size do
          val index1 = availableSlotArray(i)
          val index2 = currentFileLocationIndexes(i)
          swapElements(blocks, index1, index2)

        for usedIndex <- availableSlotArray do

          freeIndexes -= usedIndex

        if debug then printBlocks(blocks)

  blocks.zipWithIndex
    .map((maybeInt, i) => maybeInt.getOrElse(0).toLong * i.toLong)
    .sum
