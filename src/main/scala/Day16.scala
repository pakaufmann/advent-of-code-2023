import Day16.Directions.{DOWN, Direction, LEFT, RIGHT, UP}

import scala.collection.mutable
import scala.io.Source

object Day16 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day16.txt").getLines().toList

    println(part1(parse(input)))
    println(part2(parse(input)))
  }

  def part1(obstacles: Map[Coordinate, Char]): Int =
    calcEnergized(obstacles, Coordinate(0, 0), RIGHT)

  def part2(obstacles: Map[Coordinate, Char]): Int = {
    val maxX = obstacles.map(_._1.x).max
    val maxY = obstacles.map(_._1.y).max

    var maxEnergized = 0
    for (y <- 0 to maxY) {
      maxEnergized = maxEnergized.max(calcEnergized(obstacles, Coordinate(0, y), RIGHT))
      maxEnergized = maxEnergized.max(calcEnergized(obstacles, Coordinate(maxX, y), LEFT))
    }

    for (x <- 0 to maxX) {
      maxEnergized = maxEnergized.max(calcEnergized(obstacles, Coordinate(x, 0), DOWN))
      maxEnergized = maxEnergized.max(calcEnergized(obstacles, Coordinate(x, maxY), UP))
    }

    maxEnergized
  }

  private def calcEnergized(obstacles: Map[Coordinate, Char], start: Coordinate, startDir: Direction) = {
    val beams = mutable.Queue[(Coordinate, Direction)]((start, startDir))
    val visited = mutable.Set[(Coordinate, Direction)]()
    val maxX = obstacles.map(_._1.x).max
    val maxY = obstacles.map(_._1.y).max

    while (beams.nonEmpty) {
      val (pos, dir) = beams.dequeue()
      val key = (pos, dir)

      if (pos.x >= 0 && pos.x <= maxX && pos.y >= 0 && pos.y <= maxY && !visited.contains(key)) {
        visited.add(key)

        (obstacles.get(pos), dir) match {
          case (Some('|'), LEFT | RIGHT) => beams.appendAll(Seq(
            (move(pos, UP), UP),
            (move(pos, DOWN), DOWN))
          )
          case (Some('-'), UP | DOWN) => beams.appendAll(Seq(
            (move(pos, LEFT), LEFT),
            (move(pos, RIGHT), RIGHT))
          )
          case (Some('/'), LEFT) => beams.append((move(pos, DOWN), DOWN))
          case (Some('/'), RIGHT) => beams.append((move(pos, UP), UP))
          case (Some('/'), UP) => beams.append((move(pos, RIGHT), RIGHT))
          case (Some('/'), DOWN) => beams.append((move(pos, LEFT), LEFT))
          case (Some('\\'), LEFT) => beams.append((move(pos, UP), UP))
          case (Some('\\'), RIGHT) => beams.append((move(pos, DOWN), DOWN))
          case (Some('\\'), UP) => beams.append((move(pos, LEFT), LEFT))
          case (Some('\\'), DOWN) => beams.append((move(pos, RIGHT), RIGHT))
          case (_, _) => beams.append((move(pos, dir), dir))
        }
      }
    }

    visited.map(_._1).size
  }

  def move(pos: Coordinate, dir: Direction): Coordinate =
    dir match {
      case RIGHT => pos.copy(x = pos.x + 1)
      case LEFT => pos.copy(x = pos.x - 1)
      case UP => pos.copy(y = pos.y - 1)
      case DOWN => pos.copy(y = pos.y + 1)
    }

  def parse(input: List[String]): Map[Coordinate, Char] = {
    input.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.flatMap {
        case (char, x) => char match {
          case '.' => None
          case _ => Some((Coordinate(x, y), char))
        }
      }
    }.toMap
  }

  object Directions extends Enumeration {
    type Direction = Value

    val RIGHT, LEFT, UP, DOWN = Value
  }
}
