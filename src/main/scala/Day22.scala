import scala.collection.mutable
import scala.io.Source

object Day22 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day22.txt").getLines().toList

    val bricks = parse(input)
    println(part1(bricks))
    println(part2(bricks))
  }

  def part1(bricks: List[Brick]): Int = {
    val sorted = bricks.sortBy(_.coordinates.map(_.z).min)
    val settledPositions = settle(sorted)
    val mustKeep = findMustKeeps(settledPositions)
    settledPositions.size - mustKeep.size
  }

  def part2(bricks: List[Brick]): Int = {
    val sorted = bricks.sortBy(_.coordinates.map(_.z).min)
    val settledPositions = settle(sorted)
    val mustKeep = findMustKeeps(settledPositions)

    mustKeep.toList.map { brick =>
      val without = settledPositions - brick
      val moved = settle(without.toList.sortBy(_.coordinates.map(_.z).min))
      moved.diff(settledPositions).size
    }.sum
  }

  private def findMustKeeps(settledPositions: Set[Brick]): Set[Brick] = {
    val coordsToBrick = settledPositions
      .flatMap(brick => brick.coordinates.map(coord => (coord, brick)))
      .toMap

    // find bricks supporting other bricks
    val bricksSupportedBy = settledPositions.map { brick =>
      val posAt = brick.coordinates.map(c => (c.xy(), c.z)).groupBy(_._1)
      val supportedBy = posAt.flatMap { case (coordinate, pos) =>
        val minZ = pos.map(_._2).min - 1
        coordsToBrick.get(Coordinate3D.from(coordinate, minZ))
      }.toSet
      (brick, supportedBy)
    }.toMap

    bricksSupportedBy
      .filter(_._2.size == 1)
      .values
      .flatten
      .toSet
  }

  private def settle(sorted: List[Brick]): Set[Brick] = {
    val finalPositions = mutable.Set[Brick]()
    val maxPositionsAt = mutable.Map[Coordinate, Int]()

    for (brick <- sorted) {
      val moveBy = brick.coordinates.map(coordinate => {
        val nextLower = maxPositionsAt.getOrElse(coordinate.xy(), 0)
        coordinate.z - nextLower
      }).min - 1

      val settledBrick = Brick(brick.id, brick.coordinates.map(c => c.copy(z = c.z - moveBy)))
      finalPositions.add(settledBrick)

      maxPositionsAt.addAll(settledBrick.coordinates.toList
        .map(c => (c.xy(), c.z))
        .groupBy(_._1)
        .view
        .mapValues(height => height.map(_._2).max))
    }
    finalPositions.toSet
  }

  def parse(input: List[String]): List[Brick] = {
    input.zipWithIndex.map { case (line, i) =>
      val from :: to :: Nil = line.split("~").toList

      val start = parseCoordinate(from)
      val end = parseCoordinate(to)

      val coordinates = mutable.Set[Coordinate3D]()

      for (x <- start.x to end.x) {
        for (y <- start.y to end.y) {
          for (z <- start.z to end.z) {
            coordinates.add(Coordinate3D(x, y, z))
          }
        }
      }

      Brick(i, coordinates.toSet)
    }
  }

  def parseCoordinate(input: String): Coordinate3D = {
    val parts = input.split(",")

    Coordinate3D(parts(0).toInt, parts(1).toInt, parts(2).toInt)
  }
}

case class Coordinate3D(x: Int, y: Int, z: Int) {
  def xy() = Coordinate(x, y)
}

object Coordinate3D {
  def from(coord: Coordinate, z: Int): Coordinate3D = Coordinate3D(coord.x, coord.y, z)
}

case class Brick(id: Int, coordinates: Set[Coordinate3D])