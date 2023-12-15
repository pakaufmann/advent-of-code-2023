import scala.collection.mutable
import scala.io.Source

object Day15 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day15.txt").getLines().toList

    println(part1(parse(input)))
    println(part2(parse(input)))
  }

  def part1(steps: List[String]): Int =
    steps.map(hashStep).sum

  def part2(steps: List[String]): Int = {
    val ops = steps.map[Op](step => {
      if (step.endsWith("-")) {
        Remove(step.dropRight(1))
      } else {
        Add(step.dropRight(2), step.takeRight(1).toInt)
      }
    })

    val boxes = Array.fill(256) {
      mutable.Buffer[(String, Int)]()
    }

    for (op <- ops) {
      op match {
        case Add(label, focalLength) =>
          val index = boxes(op.hash).indexWhere(_._1 == label)
          if (index == -1) {
            boxes(op.hash).append((label, focalLength))
          } else {
            boxes(op.hash).update(index, (label, focalLength))
          }
        case Remove(label) =>
          boxes(op.hash) = boxes(op.hash).filterNot(_._1 == label)
      }
    }

    boxes.zipWithIndex.flatMap { case (lenses, i) =>
      lenses.zipWithIndex.map { case ((_, focalLength), slot) =>
        (i + 1) * (slot + 1) * focalLength
      }
    }.sum
  }

  def hashStep(step: String): Int =
    step.foldLeft(0) { (n, c) => ((n + c.toInt) * 17) % 256 }

  def parse(lines: List[String]): List[String] =
    lines.head.split(',').toList

  sealed trait Op {
    val hash: Int
  }

  case class Add(label: String, focalLength: Int) extends Op {
    override val hash: Int = hashStep(label)
  }

  case class Remove(label: String) extends Op {
    override val hash: Int = hashStep(label)
  }
}