import scala.collection.mutable
import scala.io.Source

object Day11 {

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day11.txt").getLines().toList

    println(sumDistances(parse(input, 2)))
    println(sumDistances(parse(input, 1000000)))
  }

  def sumDistances(galaxies: Set[Coordinate]): Long =
    galaxies.toSeq.combinations(2).map[Long] {
      case f :: s :: Nil => (s.x - f.x).abs + (s.y - f.y).abs
    }.sum

  def parse(input: List[String], expandBy: Int = 1): Set[Coordinate] = {
    var x = 0
    var y = 0

    val emptyCols = input.map(_.toCharArray).transpose.zipWithIndex.filter {
      case (col, _) => col.forall(_ == '.')
    }.map(_._2)

    val galaxies = mutable.Set[Coordinate]()

    for (row <- input) {
      x = 0

      var hasGalaxy = false
      for ((char, originalX) <- row.zipWithIndex) {
        if (char == '#') {
          galaxies.add(Coordinate(x, y))
          hasGalaxy = true
        }

        if (emptyCols.contains(originalX)) x += expandBy else x += 1
      }
      if (!hasGalaxy) y += expandBy else y += 1
    }

    galaxies.toSet
  }
}
