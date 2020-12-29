val input = scala.io.Source.fromResource(s"advent2015/day05.txt").getLines().toList

val vowels    = Set('a', 'e', 'i', 'o', 'u')
val forbidden = Set("ab", "cd", "pq", "xy")

@annotation.tailrec
def isNice(str: String, vowelCount: Int = 0, hasDuplicates: Boolean = false, hasForbidden: Boolean = false): Boolean =
  if (str.isEmpty) vowelCount >= 3 && hasDuplicates && !hasForbidden
  else
    isNice(
      str.tail,
      vowelCount + str.headOption.count(vowels),
      hasDuplicates || (str.length >= 2 && str(0) == str(1)),
      hasForbidden || forbidden(str.take(2))
    )

input.count(isNice(_))

// via regex
val hasVowels     = ".*[aeiou].*[aeiou].*[aeiou].*".r
val hasDuplicates = ".*([a-z])\\1.*".r
val hasForbidden  = ".*(ab|cd|pq|xy).*".r
input.count(name => hasVowels.matches(name) && hasDuplicates.matches(name) && !hasForbidden.matches(name))

// part 2
@annotation.tailrec
def notNaughty(
  str: String,
  hasAxA: Boolean = false,
  hasPairs: Boolean = false,
  pairs: Set[String] = Set.empty[String]
): Boolean =
  if (str.isEmpty) hasAxA && hasPairs
  else
    str.takeWhile(_ == str.head) match {
      case sub if sub.length > 2 =>
        notNaughty(str.drop(2), hasAxA = true, hasPairs, pairs + str.take(2))
      case _ =>
        notNaughty(
          str.tail,
          hasAxA || (if (str.length > 2) str(0) == str(2) else false),
          hasPairs || (if (str.length > 1) pairs(str.take(2)) else false),
          if (str.length > 1) pairs + str.take(2) else pairs
        )
    }

input.count(notNaughty(_))

// via regex
val hasPairs = ".*(..).*\\1.*".r
val hasAxA   = ".*(.).\\1.*".r

input.count(name => hasPairs.matches(name) && hasAxA.matches(name))
