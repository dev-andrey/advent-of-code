val pairs = scala.io.Source
  .fromResource(s"advent2022/day04.txt")
  .getLines()
  .toSeq
  .map(_.split(','))
  .map(arr => (arr.head.split('-'), arr.last.split('-')))
  .map { case (left, right) => (left.head.toInt -> left.last.toInt, right.head.toInt -> right.last.toInt) }

// part 1
pairs
  .count { case (leftMin -> leftMax, rightMin -> rightMax) =>
    leftMin <= rightMin && leftMax >= rightMax || leftMin >= rightMin && leftMax <= rightMax
  }

// part 2
pairs
  .count { case (leftMin -> leftMax, rightMin -> rightMax) =>
    leftMax >= rightMin && leftMax <= rightMax || rightMax >= leftMin && rightMax <= leftMax
  }
