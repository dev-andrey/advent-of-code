val input = scala.io.Source.fromResource(s"advent2020/day04.txt").getLines()

val required       = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
val optional       = Set("cid")
val validEyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
input
  .foldLeft(List(Map.empty[String, String])) {
    case result -> line if line.isEmpty =>
      Map.empty[String, String] :: result
    case result -> line =>
      result.head ++ line.split(" ").map(_.split(":")).map(kv => kv.head -> kv.last).toMap :: result.tail
  }
  .filter(_.keySet.intersect(required).size == 7)
  .filter(_.get("byr").map(_.toInt).exists(byr => byr >= 1920 && byr <= 2002))
  .filter(_.get("iyr").map(_.toInt).exists(iyr => iyr >= 2010 && iyr <= 2020))
  .filter(_.get("eyr").map(_.toInt).exists(eyr => eyr >= 2020 && eyr <= 2030))
  .filter(_.get("hcl").exists(hcl => hcl.startsWith("#") && hcl.tail.matches("[0-9a-f]+")))
  .filter(_.get("ecl").exists(validEyeColors))
  .filter(_.get("pid").exists(pid => pid.matches("[0-9]{9}")))
  .filter {
    _.get("hgt").exists(hgt =>
      hgt.takeWhile(_.isDigit).toInt match {
        case height if hgt.contains("cm") => height >= 150 && height <= 193
        case height if hgt.contains("in") => height >= 59 && height <= 76
        case _                            => false
      }
    )
  }
  .size
