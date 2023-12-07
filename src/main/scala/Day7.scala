import scala.io.Source

object Day7 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day7.txt").getLines().toList

    println(part1(parse(input)))
    println(part1(parse(input, joker = true)))
  }

  def part1(games: List[Hand]): Int = {
    games.sortWith((l, r) => r.isBetterThan(l))
      .zipWithIndex
      .map { case (card, i) => card.bid * (i + 1) }
      .sum
  }

  def parse(input: List[String], joker: Boolean = false): List[Hand] = {
    input.map(line => {
      val cards :: bid :: Nil = line.split(" ").toList

      Hand(cards.map(card => {
        if (card.isDigit) {
          card.asDigit
        } else {
          card match {
            case 'T' => 10
            case 'J' => if (joker) 1 else 11
            case 'Q' => 12
            case 'K' => 13
            case 'A' => 14
          }
        }
      }).toList, bid.toInt)
    })
  }
}

case class Hand(cards: List[Int], bid: Int) {

  private val cardGroups = cards
    .groupBy(identity)
    .view
    .mapValues(_.size)

  private val jokers = cardGroups.getOrElse(1, 0)

  private var grouped = cardGroups.filterKeys(_ != 1).toMap
  grouped = grouped
    .maxByOption(_._2)
    .map(largest =>
      grouped.updatedWith(largest._1) { a =>
        Some(a.getOrElse(0) + jokers)
      }
    )
    .getOrElse(grouped)

  val score: Int = if (grouped.size == 1 || jokers == 5) {
    7
  } else if (grouped.values.exists(_ == 4)) {
    6
  } else if (grouped.values.exists(_ == 3) && grouped.values.exists(_ == 2)) {
    5
  } else if (grouped.values.exists(_ == 3)) {
    4
  } else if (grouped.values.count(_ == 2) == 2) {
    3
  } else if (grouped.values.exists(_ == 2)) {
    2
  } else {
    1
  }

  def isBetterThan(other: Hand): Boolean = {
    if (score == other.score) {
      cards.zip(other.cards)
        .find { case (a, b) => a != b }
        .exists { case (a, b) => a > b }
    } else {
      score > other.score
    }
  }
}
