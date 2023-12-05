import scala.collection.immutable.NumericRange
import scala.collection.mutable
import scala.io.Source

object Day5 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day5.txt").getLines().toList

    val (seeds, mappings) = parse(input)

    println(part1(seeds, mappings))
    println(part2(seeds, mappings))
  }

  def part1(seeds: List[Long], mappings: List[Mapping]): Long = {
    seeds.map { seed =>
      mappings.foldLeft(seed) { (last, mapping) =>
        mapping.convert(last)
      }
    }.min
  }

  def part2(seeds: List[Long], mappings: List[Mapping]): Long = {
    def findMin(first: Long, length: Long): Long = {
      if (length == 1) {
        return calculateLocation(mappings, first).min(calculateLocation(mappings, first + 1))
      }

      val stepCount = length / 2
      val stepCount2 = length - stepCount
      val startLoc = calculateLocation(mappings, first)
      val middleLoc = calculateLocation(mappings, first + stepCount)
      val endLoc = calculateLocation(mappings, first + length)

      var min = Long.MaxValue
      if (startLoc + stepCount != middleLoc) {
        min = min.min(findMin(first, stepCount))
      }
      if (middleLoc + stepCount2 != endLoc) {
        min = min.min(findMin(first + stepCount, stepCount2))
      }
      min
    }

    seeds.sliding(2, 2).map(n => {
      findMin(n.head, n.last)
    }).min
  }

  private def calculateLocation(mappings: List[Mapping], step: Long) = {
    mappings.foldLeft(step) { (last, mapping) =>
      mapping.convert(last)
    }
  }

  def parse(input: List[String]): (List[Long], List[Mapping]) = {
    val seeds = input.head
      .replace("seeds: ", "")
      .split(" ")
      .map(_.toLong)

    val mappings = parseMappings(input.drop(2))

    (seeds.toList, mappings)
  }

  def parseMappings(input: List[String]): List[Mapping] = {
    if (input.isEmpty) {
      return List()
    }

    val mapping = parseMapping(input.takeWhile(_ != ""))

    mapping +: parseMappings(input.dropWhile(_ != "").drop(1))
  }

  def parseMapping(input: List[String]): Mapping = {
    Mapping(input.drop(1).map(range => {
      val items = range.split(" ")
      MappingRange(items(0).toLong, items(1).toLong, items(2).toLong)
    }))
  }
}

case class Mapping(ranges: List[MappingRange]) {
  def convert(last: Long): Long = {
    ranges
      .flatMap(_.convert(last))
      .headOption
      .getOrElse(last)
  }
}

case class MappingRange(destinationStart: Long, sourceStart: Long, length: Long) {
  def convert(input: Long): Option[Long] = if (sourceStart <= input && sourceStart + length >= input) {
    Some(destinationStart + (input - sourceStart))
  } else {
    None
  }

}