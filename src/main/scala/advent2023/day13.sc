val lines = scala.io.Source.fromResource(s"advent2023/day13.txt").mkString.split("\n\n")

val patterns = lines.map(_.split("\n").toSeq)

def verticalMirror(idx: Int, left: Int, right: Int, rows: Seq[String]): Option[Int] =
  if left < 0 || right == rows.head.length then Some(idx)
  else if rows.forall(columns => columns(left) == columns(right)) then verticalMirror(idx, left - 1, right + 1, rows)
  else None

def horizontalMirror(idx: Int, top: Int, bottom: Int, rows: Seq[String]): Option[Int] =
  if top < 0 || bottom == rows.length then Some(idx)
  else if rows(top) == rows(bottom) then horizontalMirror(idx, top - 1, bottom + 1, rows)
  else None

def findValue(pattern: Seq[String], excluding: Int => Boolean = _ => true): Option[Int] =
  (0 to pattern.head.length - 2)
    .flatMap(idx => verticalMirror(idx + 1, idx, idx + 1, pattern).filter(excluding))
    .headOption
    .orElse(
      (0 to pattern.length - 2)
        .flatMap(idx => horizontalMirror(idx + 1, idx, idx + 1, pattern).map(_ * 100).filter(excluding))
        .headOption
    )

val part1 = {
  for
    pattern <- patterns
    value <- findValue(pattern)
  yield value
}.sum

def flipOneCharacter(pattern: Seq[String]): Seq[Seq[String]] =
  for
    row       <- pattern.indices
    col       <- pattern.head.indices
    newPattern = pattern.updated(row, pattern(row).updated(col, if pattern(row)(col) == '.' then '#' else '.'))
  yield newPattern

val part2 = {
  for
    origin        <- patterns
    originalValue <- findValue(origin)
    thePattern    <-
      flipOneCharacter(origin).flatMap(pattern => findValue(pattern, _ != originalValue)).headOption
  yield thePattern
}.sum
