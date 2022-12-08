val input = scala.io.Source.fromResource(s"advent2022/day07.txt").getLines().toSeq

@scala.annotation.tailrec
def process(
  commands: Seq[String],
  currPath: String,
  fs: Map[String, (Seq[String], Seq[(String, Long)])]
): Map[String, (Seq[String], Seq[(String, Long)])] =
  if (commands.isEmpty) fs
  else
    commands.head match {
      case s"$$ ls" =>
        val (content, remaining) = commands.tail.span(_.head != '$')
        val (dirs, files)        = content.partition(_.startsWith("dir"))

        val curDir = currPath -> (
          dirs.map(_.split(' ')).map(name => s"${currPath.stripSuffix("/")}/${name.last}"),
          files.map(_.split(' ')).map(file => (s"${currPath.stripSuffix("/")}/${file.last}", file.head.toLong))
        )
        process(remaining, currPath, fs + curDir)

      case s"$$ cd .." =>
        val upper = currPath.dropRight(currPath.length - currPath.lastIndexOf("/"))
        process(commands.tail, if (upper.isEmpty) "/" else upper, fs)

      case s"$$ cd ${dir}" =>
        process(commands.tail, s"${currPath.stripSuffix("/")}/$dir", fs)
    }

val fs = process(input.tail, "/", Map.empty)

def calc(path: String): Seq[(String, Long)] = {
  val (dirs, files) = fs(path)

  val filesTotal = files.map(_._2).sum

  val subDirs     = dirs.flatMap(calc)
  val subDirTotal = subDirs.collect { case (path, size) if dirs.contains(path) => size }.sum

  val total = filesTotal + subDirTotal

  subDirs :+ (path -> total)
}

val calculated = calc("/")
val total      = calculated.filter(_._1 == "/").head._2

// part 1
calculated.collect { case (_, size) if size <= 100_000L => size }.sum

// part 2
calculated.collect { case (_, size) if size >= total - 40_000_000L => size }.min
