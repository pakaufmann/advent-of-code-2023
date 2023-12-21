import scala.collection.mutable
import scala.io.Source

object Day21 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day21.txt").getLines().toList

    val (gardenPlots, start) = parse(input)

    println(part1(gardenPlots, start))
    println(part2(input, 26501365))
  }

  def part2(input: List[String], steps: Int): Long = {
    val i = 2
    val i2 = i * 2 + 1

    val expanded = input.map(line => Iterator
      .continually(line
        .replace("S", ".")).take(i2).toList.mkString)

    val expandedCenter = input.map(line =>
      (Iterator.continually(line.replace("S", ".")).take(i).toList ++
        line ++
        Iterator.continually(line.replace("S", ".")).take(i).toList).mkString
    )

    val (gardenPlots, start) = parse(
      Iterator.continually(expanded).take(i).toList.flatten ++
        expandedCenter ++
        Iterator.continually(expanded).take(i).toList.flatten
    )

    val x = part1(gardenPlots, start, 65)
    val x2 = part1(gardenPlots, start, 65 + 131)
    val x3 = part1(gardenPlots, start, 65 + (131 * 2))
    val find = steps / ((gardenPlots.map(_.x).max / i2) + 1)

    // quadratic interpolation magic: https://www.bbc.co.uk/bitesize/guides/zy82ng8/revision/4
    // or by hand:
    // 3791 33646 93223
    //  29855 59577
    //    29722
    //
    // 14861*x2
    //
    // 3791  33646   93223
    // 0     14861   59444
    // +3791 +18785  +33779
    //  14994   14994
    //
    // 14994*x
    //
    // 3791  18785   33779
    // 0     14994   29988
    // +3791 +3791   +3791
    //
    //14861*x2 + 14994*x + 3791
    fitQuadratic(x, x2, x3).apply(find)
  }

  def fitQuadratic(p1: Long, p2: Long, p3: Long): Int => Long = {
    x =>
      (p1 * (x - 1) * (x - 2)) / 2 +
        -(p2 * x * (x - 2)) +
        (p3 * x * (x - 1)) / 2
  }

  def part1(gardenPlots: Set[Coordinate], start: Coordinate, totalStepCount: Int = 64): Int = {
    val queue = mutable.Queue((start, 0))
    val visited = mutable.Map[Coordinate, Int]()

    while (queue.nonEmpty) {
      val (pos, stepCount) = queue.dequeue()
      if (stepCount <= totalStepCount && !visited.contains(pos)) {
        visited.put(pos, stepCount)
        queue.addAll(pos.fourNeighbours().intersect(gardenPlots).map(p => (p, stepCount + 1)))
      }
    }

    visited.count(_._2 % 2 == totalStepCount % 2)
  }

  def parse(lines: List[String]): (Set[Coordinate], Coordinate) = {
    var start: Option[Coordinate] = None

    val gardenPlots = lines.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.flatMap { case (c, x) =>
        c match {
          case '.' => Some(Coordinate(x, y))
          case 'S' =>
            start = Some(Coordinate(x, y))
            Some(Coordinate(x, y))
          case _ => None
        }
      }
    }

    (gardenPlots.toSet, start.get)
  }
}
