import Day16.Directions.{DOWN, Direction, LEFT, RIGHT, UP}

import scala.collection.mutable
import scala.io.Source

object Day18 {
  val parseRegex = """(.) ([0-9]+) (\(.*\))""".r

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day18.txt").getLines().toList

    println(part1(parse(input)))
    println(part2(parse(input)))
  }

  def part2(instructions: List[DigInstruction]): Long =
    calc(instructions, _.direction2, _.meter2)

  def part1(instructions: List[DigInstruction]): Long =
    calc(instructions, _.direction, _.meter)


  private def calc(instructions: List[DigInstruction], dirFunc: DigInstruction => Direction, meterFunc: DigInstruction => Long) = {
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

    // Shoelace formula & picks theorem
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
