val input = scala.io.Source.fromResource(s"advent2020/day02.txt").getLines().toList

val parsedInput = input
  .map(str => str.split(' '))
  .map(struct => (struct.head.split('-').map(_.toInt), struct(1).charAt(0), struct.last))

parsedInput.count { case (minMax, char, password) =>
  password.count(_ == char) match {
    case count => (minMax.head <= count) && (count <= minMax.last)
  }
}

parsedInput.count { case (yesNo, char, password) =>
  (password(yesNo.head - 1), password(yesNo.last - 1)) match {
    case (ch1, ch2) => ch1 != ch2 && (ch1 == char || ch2 == char)
  }
}
