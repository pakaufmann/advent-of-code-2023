import scala.io.Source

object Day3 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day3.txt").getLines().toList

    val (symbols, numbers) = parse(input)

    println(part1(symbols, numbers))
    println(part2(symbols, numbers))
  }

  private def part1(symbols: Set[Symbol], numbers: List[Number]): Int = {
    val coordinates = symbols.map(_.coordinate)
    numbers.filter(_.adjacentTo(coordinates).nonEmpty).map(_.number).sum
  }

  private def part2(symbols: Set[Symbol], numbers: List[Number]): Int = {
    val gears = symbols.filter(_.symbol == '*').map(_.coordinate)

    numbers
      .foldLeft(Map[Coordinate, Set[Number]]()) { (adjacent, number) =>
        adjacent ++ number.adjacentTo(gears)
          .map(gear => (gear, adjacent.getOrElse(gear, Set[Number]()) + number))
          .toMap
      }
      .filter(_._2.size == 2)
      .map(a => a._2.head.number * a._2.last.number)
      .sum
  }

  def parse(input: List[String]): (Set[Symbol], List[Number]) =
    input.zipWithIndex
      .map(a => parseLine(a._1, a._2))
      .reduce((a, b) => (a._1 ++ b._1, a._2 ++ b._2))

  def parseLine(line: String, y: Int): (Set[Symbol], List[Number]) = {
    var number = Number(0, Set())
    var symbols = Set[Symbol]()
    var numbers = List[Number]()

    for ((c, x) <- line.zipWithIndex) {
      val coordinate = Coordinate(x, y)

      if (c.isDigit) {
        number = number.add(c.asDigit, coordinate)
      } else {
        if (c != '.') {
          symbols = symbols + Symbol(c, coordinate)
        }
        if (number.coordinates.nonEmpty) {
          numbers = numbers :+ number
          number = Number(0, Set())
        }
      }
    }

    if (number.coordinates.nonEmpty) {
      numbers = numbers :+ number
    }

    (symbols, numbers)
  }
}

case class Symbol(symbol: Char, coordinate: Coordinate)

case class Coordinate(x: Int, y: Int) {
  def neighbours(): Set[Coordinate] =
    (-1 to 1).flatMap(xD => (-1 to 1).map(yD => Coordinate(x + xD, y + yD))).toSet - this

  def fourNeighbours(): Set[Coordinate] =
    Set(
      copy(x = x + 1),
      copy(x = x - 1),
      copy(y = y + 1),
      copy(y = y - 1),
    )
}

case class Number(number: Int, coordinates: Set[Coordinate]) {
  def add(digit: Int, coordinate: Coordinate): Number =
    Number(number * 10 + digit, coordinates + coordinate)

  def adjacentTo(symbols: Set[Coordinate]): Set[Coordinate] =
    symbols.intersect(coordinates.flatMap(_.neighbours()))
}
