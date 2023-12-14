import scala.collection.mutable
import scala.io.Source

object Day14 {

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day14.txt").getLines().toList

    val (round, cubes) = parse(input)
    val width = input.head.length

    println(part1(round, cubes, input.size, width))
    println(part2(round, cubes, input.size, width))
  }

  def part1(round: Set[Coordinate], cubes: Set[Coordinate], height: Int, width: Int): Int =
    calc(runRound(round, cubes, height, width), width)

  def part2(roundRocks: Set[Coordinate], cubeRocks: Set[Coordinate], height: Int, width: Int): Int = {
    val northCubes = turnRight(cubeRocks, height)
    val westCubes = turnRight(northCubes, width)
    val southCubes = turnRight(westCubes, height)

    var startRound = roundRocks

    val previousRounds = mutable.Map[Set[Coordinate], Int]()
    previousRounds.put(startRound, 0)
    var at = 0

    while (true) {
      val northRound = runRoundTurn(startRound, cubeRocks, height, width)
      val westRound = runRoundTurn(northRound, northCubes, width, height)
      val southRound = runRoundTurn(westRound, westCubes, height, width)
      val eastRound = runRoundTurn(southRound, southCubes, width, height)

      startRound = eastRound
      at += 1
      if (previousRounds.contains(startRound)) {
        val previousAt = previousRounds(startRound)
        val loopSize = at - previousAt
        val remaining = 1000000000 - at
        val finalLoop = remaining % loopSize

        val roundToConstellation = previousRounds.map(k => (k._2, k._1)).toMap
        return calc(roundToConstellation(previousAt + finalLoop), width)
      } else {
        previousRounds.put(startRound, at)
      }
    }

    0
  }

  def calc(round: Set[Coordinate], width: Int): Int = round.toList.map(width - _.y).sum

  def runRoundTurn(round: Set[Coordinate], cubes: Set[Coordinate], height: Int, width: Int): Set[Coordinate] =
    turnRight(runRound(round, cubes, height, width), height)

  def turnRight(end: Set[Coordinate], height: Int): Set[Coordinate] =
    end.map(c => Coordinate(x = height - 1 - c.y, y = c.x))

  private def runRound(round: Set[Coordinate], cubes: Set[Coordinate], height: Int, width: Int) = {
    val finalPosition = mutable.Set[Coordinate]()

    for (x <- 0 to height) {
      var to = 0

      for (y <- 0 to width) {
        val coordinate = Coordinate(x, y)

        if (cubes.contains(coordinate)) {
          to = coordinate.y + 1
        }
        if (round.contains(coordinate)) {
          finalPosition.add(Coordinate(x, to))
          to += 1
        }
      }
    }

    finalPosition.toSet
  }

  def parse(input: List[String]): (Set[Coordinate], Set[Coordinate]) = {
    val roundRocks = mutable.Set[Coordinate]()
    val cubeRocks = mutable.Set[Coordinate]()

    for ((line, y) <- input.zipWithIndex) {
      for ((char, x) <- line.zipWithIndex) {
        char match {
          case 'O' => roundRocks.add(Coordinate(x, y))
          case '#' => cubeRocks.add(Coordinate(x, y))
          case _ =>
        }
      }
    }

    (roundRocks.toSet, cubeRocks.toSet)
  }
}
