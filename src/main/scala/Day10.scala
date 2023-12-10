import Connections.{Connection, East, North, South, West}

import scala.collection.mutable
import scala.io.Source

object Day10 {

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day10.txt").getLines().toList

    val map = parse(input)

    println(part1(map))
    println(part2(map))
  }

  def part1(pipes: Map[Coordinate, Pipe]): Int = {
    val startPipe = pipes.find(_._2.connects.size == 4).get
      ._2

    val validNeighbours = startPipe
      .neighbours()
      .flatMap(n => pipes.get(n))

    validNeighbours.flatMap(loop(startPipe, _, pipes).map(_.size + 1)).max / 2
  }

  def part2(pipes: Map[Coordinate, Pipe]): Int = {
    val startPipe = pipes.find(_._2.connects.size == 4).get
      ._2

    val validNeighbours = startPipe
      .neighbours()
      .flatMap(n => pipes.get(n))

    val loops = validNeighbours.flatMap(loop(startPipe, _, pipes))
      .maxBy(_.size + 1)
      .map(_.coordinate)

    val xCoords = loops.map(_.x)
    val yCoords = loops.map(_.y)

    val minX = xCoords.min
    val maxX = xCoords.max
    val minY = yCoords.min
    val maxY = yCoords.max

    (minY to maxY).map { y =>
      (minX to maxX).map(Coordinate(_, y)).foldLeft((false, 0)) { case ((enclosed, cnt), coord) =>
        if (loops.contains(coord)) {
          if (pipes(coord).connects.contains(South)) {
            (!enclosed, cnt)
          } else {
            (enclosed, cnt)
          }
        } else {
          (enclosed, if (enclosed) cnt + 1 else cnt)
        }
      }._2
    }.sum
  }

  def loop(startPipe: Pipe, from: Pipe, map: Map[Coordinate, Pipe]): Option[Set[Pipe]] = {
    var last = startPipe
    var at: Option[Pipe] = Some(from)
    val visited = mutable.Set[Pipe](startPipe)

    while (at.isDefined) {
      val current = at.get

      val neighbours = current.neighbours().filter(_ != last.coordinate)
        .flatMap(map.get)

      val next = neighbours.headOption
      visited.add(current)

      if (next.isDefined) {
        if (next.get == startPipe) {
          return Some(visited.toSet)
        }

        last = current
        at = next
      } else {
        return None
      }
    }

    None
  }

  def parse(input: List[String]): Map[Coordinate, Pipe] =
    input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.filter(_._1 != '.').map { case (c, x) =>
        c match {
          case '|' => Pipe(List(North, South), Coordinate(x, y))
          case '-' => Pipe(List(East, West), Coordinate(x, y))
          case 'L' => Pipe(List(North, East), Coordinate(x, y))
          case 'J' => Pipe(List(North, West), Coordinate(x, y))
          case '7' => Pipe(List(South, West), Coordinate(x, y))
          case 'F' => Pipe(List(South, East), Coordinate(x, y))
          case 'S' => Pipe(List(East, North, South, West), Coordinate(x, y))
        }
      }
    }.map(a => (a.coordinate, a)).toMap
}

case class Distance(pipe: Pipe, distance: Int)

object Connections extends Enumeration {
  type Connection = Value

  val North, South, East, West = Value
}

case class Pipe(connects: List[Connection], coordinate: Coordinate) {
  def neighbours(): List[Coordinate] = connects.map {
    case North => coordinate.copy(y = coordinate.y - 1)
    case South => coordinate.copy(y = coordinate.y + 1)
    case East => coordinate.copy(x = coordinate.x + 1)
    case West => coordinate.copy(x = coordinate.x - 1)
  }

  def neighboursWithConnection(): List[(Connection, Coordinate)] = connects.map {
    case North => (North, coordinate.copy(y = coordinate.y - 1))
    case South => (South, coordinate.copy(y = coordinate.y + 1))
    case East => (East, coordinate.copy(x = coordinate.x + 1))
    case West => (West, coordinate.copy(x = coordinate.x - 1))
  }
}
