import scala.io.Source

object Day2 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day2.txt").getLines().toList

    val rounds = parse(input)
    println(part1(rounds))
    println(part2(rounds))
  }

  def part1(rounds: List[Game]): Int = rounds
    .filter(_.isValid(12, 13, 14))
    .map(_.id)
    .sum

  def part2(rounds: List[Game]): Int = rounds.map(_.minPower()).sum

  def parse(input: List[String]): List[Game] = {
    input.map(parseGame)
  }

  def parseGame(line: String): Game = {
    val (game, rounds) = line.splitAt(line.indexOf(":"))
    val id = game.replace("Game ", "").toInt

    Game(id, rounds.drop(2).trim.split("; ").map(parseRound).toList)
  }

  def parseRound(round: String): Round = {
    var red = 0
    var green = 0
    var blue = 0
    for (cubes <- round.split(",")) {
      val split = cubes.trim.split(" ")
      val count = split.head.toInt

      split.last match {
        case "red" => red = count
        case "green" => green = count
        case "blue" => blue = count
      }
    }

    Round(red, green, blue)
  }
}

case class Round(red: Int, green: Int, blue: Int) {
  def isValid(maxRed: Int, maxGreen: Int, maxBlue: Int): Boolean =
    red <= maxRed && green <= maxGreen && blue <= maxBlue
}

case class Game(id: Int, rounds: List[Round]) {
  def isValid(maxRed: Int, maxGreen: Int, maxBlue: Int): Boolean =
    rounds.forall(_.isValid(maxRed, maxGreen, maxBlue))

  def minPower(): Int =
    rounds.map(_.red).max * rounds.map(_.green).max * rounds.map(_.blue).max
}
