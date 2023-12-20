import Day16.Directions.{DOWN, Direction, LEFT, RIGHT, UP}

import scala.collection.mutable
import scala.io.Source

object Day18 {
  val parseRegex = """(.) ([0-9]+) (\(.*\))""".r

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day18.txt").getLines().toList

    println(part1(parse(input)))
    println(part2(parse(input)))

    println(part1Shoelace(parse(input)))
    println(part2Shoelace(parse(input)))
  }

  def part1(instructions: List[DigInstruction]): Long =
    calc(instructions, _.direction, _.meter)

  def part2(instructions: List[DigInstruction]): Long =
    calc(instructions, _.direction2, _.meter2)

  def part1Shoelace(instructions: List[DigInstruction]): Long =
    shoelace(instructions, _.direction, _.meter)

  def part2Shoelace(instructions: List[DigInstruction]): Long =
    shoelace(instructions, _.direction2, _.meter2)

  case class LCoordinate(x: Long, y: Long)

  case class Edge(coordinate: LCoordinate, down: Long = 0, up: Long = 0, right: Long = 0, left: Long = 0)

  private def calc(instructions: List[DigInstruction], dirFunc: DigInstruction => Direction, meterFunc: DigInstruction => Long) = {
    val edges = instructions
      .scanLeft(Edge(LCoordinate(0, 0))) { (pos, instruction) =>
        val meter = meterFunc(instruction)
        dirFunc(instruction) match {
          case RIGHT => Edge(coordinate = pos.coordinate.copy(x = pos.coordinate.x + meter), left = meter)
          case LEFT => Edge(coordinate = pos.coordinate.copy(x = pos.coordinate.x - meter), right = meter)
          case UP => Edge(coordinate = pos.coordinate.copy(y = pos.coordinate.y - meter), down = meter)
          case DOWN => Edge(coordinate = pos.coordinate.copy(y = pos.coordinate.y + meter), up = meter)
        }
      }

    val withDirections = edges.sliding(2).map {
      case f :: s :: Nil =>
        if (s.up != 0) {
          f.copy(down = s.up)
        } else if (s.left != 0) {
          f.copy(right = s.left)
        } else {
          f
        }
    }.toList

    val completeDirections = withDirections.head.copy(down = edges.last.down) +: withDirections.drop(1)
    val byY = completeDirections.groupBy(_.coordinate.y)

    var currentBorders = Map[Long, Edge]()
    var total = 0L
    var lastY = 0L
    var lastCount = 0L

    for (y <- byY.keySet.flatMap(i => List(i, i + 1)).toList.sorted) {
      val diff = y - lastY
      lastY = y

      val updatedBorders = currentBorders
        .map { case (k, e) => (k, e.copy(down = e.down - diff, right = 0)) }
        .filter(_._2.down >= 0)

      currentBorders = updatedBorders ++
        byY.getOrElse(y, List()).map(n => n.coordinate.x -> n)

      var isEnclosed = false
      var enclosedCount = 0L
      var lastColumn = 0L

      for (x <- currentBorders.keys.toList.sorted) {
        val border = currentBorders(x)

        enclosedCount += 1
        if (isEnclosed) {
          enclosedCount += x - lastColumn - 1
        }

        if (border.down > 0) {
          isEnclosed = !isEnclosed
        }

        if (!isEnclosed) {
          if (border.right > 0) {
            enclosedCount += border.right - 1
          }
        }
        lastColumn = x
      }

      total += lastCount * diff
      lastCount = enclosedCount
    }
    
    total
  }

  def shoelace(instructions: List[DigInstruction], dirFunc: DigInstruction => Direction, meterFunc: DigInstruction => Long): Long = {
    // Shoelace formula & picks theorem
    val edges = instructions
      .scanLeft((0L, 0L)) { (pos, instruction) =>
        val meter = meterFunc(instruction)
        dirFunc(instruction) match {
          case RIGHT => pos.copy(_1 = pos._1 + meter)
          case LEFT => pos.copy(_1 = pos._1 - meter)
          case UP => pos.copy(_2 = pos._2 - meter)
          case DOWN => pos.copy(_2 = pos._2 + meter)
        }
      }

    (edges
      .sliding(2)
      .foldLeft(instructions.map(meterFunc).sum) { (sum, l) =>
        val first :: second :: Nil = l
        sum + (first._2 + second._2) * (first._1 - second._1)
      } / 2) + 1
  }

  def findOuterSize(minX: Int, maxX: Int, minY: Int, maxY: Int, loop: mutable.Set[Coordinate]): Int = {
    val queue = mutable.Queue(Coordinate(minX, minY))
    val visited = mutable.Set[Coordinate]()

    while (queue.nonEmpty) {
      val next = queue.dequeue()

      if (!visited.contains(next) && !loop.contains(next)) {
        visited.add(next)
        queue.addAll(next.neighbours().filter(c => c.x >= minX && c.x <= maxX && c.y >= minY && c.y <= maxY))
      }
    }

    visited.size
  }

  def parse(input: List[String]): List[DigInstruction] = {
    input.map(line => {
      val parsed = parseRegex.findAllIn(line)
      val hex = parsed.group(3)
        .replace("(", "")
        .replace(")", "")
        .replace("#", "")
      val meter2 = Integer.parseInt(hex.take(5), 16)
      val dir2 = hex.drop(5).toInt

      DigInstruction(parsed.group(1) match {
        case "L" => LEFT
        case "R" => RIGHT
        case "U" => UP
        case "D" => DOWN
      }, parsed.group(2).toInt, dir2 match {
        case 0 => RIGHT
        case 1 => DOWN
        case 2 => LEFT
        case 3 => UP
      }, meter2)
    })
  }
}

case class DigInstruction(direction: Direction, meter: Int, direction2: Direction, meter2: Long)
