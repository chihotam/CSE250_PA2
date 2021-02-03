package cse250.pa2

import org.scalatest.{BeforeAndAfter, FlatSpec}

class ComparatorTests extends FlatSpec with BeforeAndAfter {
  behavior of "NewIntOrdering"
  it should "order evens before odds" in {
    val evens = Array.tabulate(10)(2 * _)
    val odds = Array.tabulate(10)(2 * _ + 1)
    for (e <- evens; o <- odds) assert(NewIntOrdering.compare(e, o) == -1, s"Your comparator returned ${NewIntOrdering.compare(e, o)} while comparing $e and $o.")
  }

  it should "order larger evens before smaller evens" in {
    val evens = Array.tabulate(10)(2 * _)
    for (largerIndex <- evens.indices.reverse; smallerIndex <- 0 until largerIndex)
      assert(NewIntOrdering.compare(evens(largerIndex), evens(smallerIndex)) == -1, s"Your comparator returned ${NewIntOrdering.compare(evens(largerIndex), evens(smallerIndex))} while comparing ${evens(largerIndex)} and ${evens(smallerIndex)}.")
  }
}
