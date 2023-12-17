import Day16.Directions.{DOWN, Direction, LEFT, RIGHT, UP}

import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object Day17 {

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day17.txt").getLines().toList

    println(part1(parse(input)))
    println(part2(parse(input)))
  }

  def part1(map: Map[Coordinate, Int]): Int = {
    findPath(map, 1, 3)
  }

  def part2(map: Map[Coordinate, Int]): Int = {
    findPath(map, 4, 10)
  }

  def findPath(map: Map[Coordinate, Int], minLength: Int, maxLength: Int): Int = {
    var minHeat = Int.MaxValue
    val maxX = map.map(_._1.x).max
    val maxY = map.map(_._1.y).max

    val queue = mutable.PriorityQueue[State]()(StateOrdering)
    val visited = mutable.Map[(Coordinate, Direction), (Int, Int)]()

    queue.addAll(List(
      State(Coordinate(0, 0), 0, RIGHT, 0),
      State(Coordinate(0, 0), 0, DOWN, 0)
    ))

    while (queue.nonEmpty) {
      breakable {
        val state@State(coordinate, heat, direction, remaining) = queue.dequeue()
        if (coordinate.x == maxX && coordinate.y == maxY) {
          minHeat = minHeat.min(heat)
          break()
        }
        if (visited.get((coordinate, direction)).exists(a => a._1 <= heat && a._2 >= remaining)) {
          break()
        }
        if (heat >= minHeat) {
          break()
        }

        visited.put((coordinate, direction), (heat, remaining))

        val (dx1, dx2) = direction match {
          case DOWN | UP => (-1, 1)
          case LEFT | RIGHT => (0, 0)
        }
        val (dy1, dy2) = direction match {
          case DOWN | UP => (0, 0)
          case LEFT | RIGHT => (-1, 1)
        }
        val (dir1, dir2) = direction match {
          case DOWN | UP => (LEFT, RIGHT)
          case LEFT | RIGHT => (UP, DOWN)
        }

        queue.addAll(
          generateNextCoords(map, minLength, maxLength, state, dx1, dy1, dir1, maxX, maxY) ++
            generateNextCoords(map, minLength, maxLength, state, dx2, dy2, dir2, maxX, maxY))
      }
    }

    minHeat
  }

  private def generateNextCoords(map: Map[Coordinate, Int], minLength: Int, maxLength: Int, state: State, dx: Int, dy: Int, dir2: Direction, maxX: Int, maxY: Int) = {
    val baseHeat = (1 until minLength)
      .flatMap(l => map.get(Coordinate(state.coordinate.x + (dx * l), state.coordinate.y + (dy * l))))
      .sum

    (minLength to maxLength)
      .map(l => Coordinate(x = state.coordinate.x + (dx * l), y = state.coordinate.y + (dy * l)))
      .filter(c => c.x >= 0 && c.y >= 0 && c.x <= maxX && c.y <= maxY)
      .scanLeft(state.copy(heat = state.heat + baseHeat, remaining = maxLength)) { (s, c) =>
        s.copy(c, heat = s.heat + map(c), dir2, s.remaining - 1)
      }
      .drop(1)
  }

  def parse(input: List[String]): Map[Coordinate, Int] = {
    input.zipWithIndex.flatMap {
      case (line, y) => line.zipWithIndex.map {
        case (char, x) => (Coordinate(x, y), char.asDigit)
      }
    }.toMap
  }

  case class State(coordinate: Coordinate, heat: Int, direction: Direction, remaining: Int) {
    def valid(maxX: Int, maxY: Int): Boolean =
      coordinate.x >= 0 && coordinate.y >= 0 && coordinate.x <= maxX && coordinate.y <= maxY
  }

  object StateOrdering extends Ordering[State] {
    def compare(a: State, b: State) = b.heat compare a.heat
  }
}
