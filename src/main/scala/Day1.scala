import scala.io.Source
import scala.util.matching.Regex

object Day1 {
  private val numberRegex = "1|2|3|4|5|6|7|8|9".r
  private val numberTextRegex = "one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9".r
  private val reserveNumberTextRegex = "9|8|7|6|5|4|3|2|1|enin|thgie|neves|xis|evif|ruof|eerht|owt|eno".r

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day1.txt").getLines().toList

    println(find(input, numberRegex, numberRegex))
    println(find(input, numberTextRegex, reserveNumberTextRegex))
  }

  private def find(input: List[String], first: Regex, last: Regex): Int =
    input.map(findDigits(first, last)).sum

  private def findDigits(firstRegex: Regex, lastRegex: Regex)(line: String): Int =
      toNumber(firstRegex.findFirstIn(line).getOrElse("0")) * 10 +
      toNumber(lastRegex.findFirstIn(line.reverse).getOrElse("0").reverse)

  private def toNumber(s: String) =
    s match {
      case "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => s.head.asDigit
      case "one" => 1
      case "two" => 2
      case "three" => 3
      case "four" => 4
      case "five" => 5
      case "six" => 6
      case "seven" => 7
      case "eight" => 8
      case "nine" => 9
    }
}
