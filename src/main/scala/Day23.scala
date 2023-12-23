import Day16.Directions.{DOWN, Direction, LEFT, RIGHT, UP}

import scala.collection.mutable
import scala.io.Source

object Day23 {

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day23.txt").getLines().toList

    val (paths, slopes) = parse(input)

    println(part1(paths, slopes.map(s => s.position -> s.direction).toMap))
    println(part2(paths, slopes.map(s => s.position -> s.direction).toMap))
  }

  def part1(paths: Set[Coordinate], slopes: Map[Coordinate, Direction]): Int =
    findLongestPath(paths, slopes)

  def part2(paths: Set[Coordinate], slopes: Map[Coordinate, Direction]): Int =
    findLongestPathInGraph(createGraph(paths, slopes))

  private def createGraph(paths: Set[Coordinate], slopes: Map[Coordinate, Direction]): Map[Coordinate, Map[Coordinate, Int]] = {
    val nodes = paths ++ slopes.keySet
    val start = paths.minBy(_.y)
    val end = paths.maxBy(_.y)

    val graph = mutable.Map[Coordinate, Map[Coordinate, Int]]()
    val visited = mutable.Set[(Coordinate, Coordinate)]()
    val queue = mutable.Queue((start, start, 0))

    while (queue.nonEmpty) {
      val (at, from, steps) = queue.dequeue()

      if (!visited.contains(at, from)) {
        val neighbours = at.fourNeighbours().intersect(nodes)
        if (neighbours.size != 2) {
          if (from != at) {
            graph.updateWith(from) { in =>
              Some(in.getOrElse(Map()) + (at -> steps))
            }
            graph.updateWith(at) { in =>
              Some(in.getOrElse(Map()) + (from -> steps))
            }
          }

          queue.addAll(neighbours.map(n => (n, at, 1)))
        } else {
          if (neighbours.head == end) {
            graph.updateWith(from) { in =>
              Some(in.getOrElse(Map()) + (neighbours.head -> (steps + 1)))
            }
            graph.updateWith(neighbours.head) { in =>
              Some(in.getOrElse(Map()) + (from -> (steps + 1)))
            }
          } else {
            queue.addAll(neighbours.map(n => (n, from, steps + 1)))
          }
        }
      }
      visited.add(at, from)
    }
    graph.toMap
  }

  def findLongestPathInGraph(graph: Map[Coordinate, Map[Coordinate, Int]]): Int = {
    val start = graph.keySet.minBy(_.y)
    val end = graph.keySet.maxBy(_.y)

    var max = 0
    val queue = mutable.Queue((start, 0, Set[Coordinate]()))

    while (queue.nonEmpty) {
      val (next, steps, visited) = queue.dequeue()
      if (next == end) {
        max = max.max(steps)
      } else {
        queue.addAll(graph(next)
          .filter(a => !visited.contains(a._1))
          .map { case (node, stepsToNext) =>
            (node, steps + stepsToNext, visited + next)
          })
      }
    }

    max
  }

  def findLongestPath(paths: Set[Coordinate], slopes: Map[Coordinate, Direction]): Int = {
    val start = paths.minBy(_.y)
    val end = paths.maxBy(_.y)

    var max = 0
    val queue = mutable.Stack((start, 0, Set[Coordinate]()))

    while (queue.nonEmpty) {
      val (next, steps, visited) = queue.pop()
      val newVisited = visited + next

      if (next == end) {
        max = max.max(steps)
      } else {
        queue.pushAll(next.fourNeighbours()
          .diff(visited)
          .flatMap { neighbour =>
            (paths.contains(neighbour), slopes.get(neighbour)) match {
              case (true, _) => Some(neighbour, steps + 1, newVisited)
              case (_, Some(RIGHT)) =>
                val right = neighbour.right()
                if (newVisited.contains(right)) None else Some(right, steps + 2, newVisited + neighbour)
              case (_, Some(LEFT)) =>
                val left = neighbour.left()
                if (newVisited.contains(left)) None else Some(left, steps + 2, newVisited + neighbour)
              case (_, Some(UP)) =>
                val top = neighbour.top()
                if (newVisited.contains(top)) None else Some(top, steps + 2, newVisited + neighbour)
              case (_, Some(DOWN)) =>
                val bottom = neighbour.bottom()
                if (newVisited.contains(bottom)) None else Some(bottom, steps + 2, newVisited + neighbour)
              case _ => None
            }
          })
      }
    }

    max
  }

  def parse(input: List[String]): (Set[Coordinate], Set[Slope]) = {
    input.zipWithIndex.foldLeft((Set[Coordinate](), Set[Slope]())) { case (cur, (line, y)) =>
      line.zipWithIndex.foldLeft(cur) { case (cur, (c, x)) =>
        c match {
          case '.' => (cur._1 + Coordinate(x, y), cur._2)
          case '>' => (cur._1, cur._2 + Slope(Coordinate(x, y), RIGHT))
          case '<' => (cur._1, cur._2 + Slope(Coordinate(x, y), LEFT))
          case 'v' => (cur._1, cur._2 + Slope(Coordinate(x, y), DOWN))
          case '^' => (cur._1, cur._2 + Slope(Coordinate(x, y), UP))
          case _ => cur
        }
      }
    }
  }
}

case class Slope(position: Coordinate, direction: Direction)