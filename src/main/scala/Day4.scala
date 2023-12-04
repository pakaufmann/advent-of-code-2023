import scala.collection.mutable
import scala.io.Source

object Day4 {
  val inputRegex = "Card +([0-9]*): (.*) \\| (.*)".r

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day4.txt").getLines().toList

    val cards = parse(input)

    println(part1(cards))
    println(part2(cards))
  }

  private def part1(cards: List[Card]): Int =
    cards.map(_.points()).sum

  private def part2(cards: List[Card]): Int = {
    val cardCount = mutable.Map[Card, Int](cards.map((_, 1)): _*)

    for (card <- cards) {
      val from = card.id
      val count = cardCount.getOrElse(card, 1)

      for (n <- 0 until card.overlap().size) {
        cardCount.updateWith(cards(from + n)) { a =>
          Some(a.getOrElse(1) + count)
        }
      }
    }

    cardCount.values.sum
  }

  private def parse(input: List[String]): List[Card] =
    input.map(parseLine)

  private def parseLine(line: String): Card = {
    val group = inputRegex.findFirstMatchIn(line).get
    Card(group.group(1).toInt, parseNumbers(group.group(2)), parseNumbers(group.group(3)))
  }

  private def parseNumbers(numbers: String): Set[Int] =
    numbers.trim.split(" +").map(n => n.trim.toInt).toSet
}

case class Card(val id: Int, winningNumbers: Set[Int], myNumbers: Set[Int]) {
  def overlap(): Set[Int] = winningNumbers.intersect(myNumbers)

  def points(): Int = ((1 << overlap().size) / 2)
}