import scala.annotation.tailrec
import scala.io.Source

object Day13 {

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day13.txt").getLines().toList

    val patterns = parse(input)
    println(part1(patterns))
    println(part2(patterns))
  }

  def part1(patterns: List[Pattern]): Int =
    patterns.map(_.findMirror().get).sum

  def part2(patterns: List[Pattern]): Int = {
    patterns.map(findDifferentPattern).sum
  }

  def findDifferentPattern(pattern: Pattern): Int = {
    val initialPattern = pattern.findMirror()

    pattern.lines.to(LazyList).zipWithIndex.flatMap { case (line, y) =>
      line.to(LazyList).zipWithIndex.flatMap { case (char, x) =>
        val flipped = char match {
          case '#' => '.'
          case _ => '#'
        }
        Pattern(pattern.lines.updated(y, line.updated(x, flipped)))
          .findMirror(initialPattern)
          .toList
      }
    }.headOption.getOrElse(0)
  }

  def parse(input: List[String]): List[Pattern] = {
    if (input.isEmpty) return List()
    Pattern(input.takeWhile(_.nonEmpty).map(_.toCharArray.toList)) +:
      parse(input.dropWhile(_.nonEmpty).drop(1))
  }
}

case class Pattern(lines: List[List[Char]]) {
  def findMirror(ignore: Option[Int] = None): Option[Int] = {
    @tailrec
    def find(lines: List[List[Char]]): Boolean = {
      if (lines.size % 2 == 1) return false
      if (lines.isEmpty) return true

      lines.last == lines.head && find(lines.slice(1, lines.length - 1))
    }

    def mirroredAt(lines: List[List[Char]]): Option[(Int, Int)] = {
      for (index <- lines.indices) {
        if (find(lines.drop(index))) {
          return Some((lines.length - index) / 2, index)
        }
      }
      None
    }

    def withIndex(in: (Int, Int)) = in._1 + in._2

    def times100(in: Int) = in * 100

    def isIgnored(result: Int): Boolean = ignore.contains(result)

    mirroredAt(lines)
      .map(a => times100(withIndex(a)))
      .filterNot(isIgnored)
      .orElse(mirroredAt(lines.reverse).map(a => times100(a._1)).filterNot(isIgnored))
      .orElse(mirroredAt(lines.transpose).map(withIndex).filterNot(isIgnored))
      .orElse(mirroredAt(lines.transpose.reverse).map(_._1).filterNot(isIgnored))
  }
}