import scala.io.Source

object Day6 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day6.txt").getLines().toList

    val races = parse(input)
    println(part1(races))

    val races2 = parse2(input)
    println(part1(races2))
  }

  def part1(races: List[Race]): Long = {
    races.map(_.marginOfError()).product
  }

  def parse2(input: List[String]): List[Race] = {
    val times = input.head
      .replaceAll(" +", "")
      .replace("Time:", "")
      .split(" ")
    val distances = input.last
      .replaceAll(" +", "")
      .replace("Distance:", "")
      .split(" ")

    times.zip(distances).map((race) => Race(race._1.toLong, race._2.toLong)).toList
  }

  def parse(input: List[String]): List[Race] = {
    val times = input.head
      .replaceAll(" +", " ")
      .replace("Time: ", "")
      .split(" ")
    val distances = input.last
      .replaceAll(" +", " ")
      .replace("Distance: ", "")
      .split(" ")


    times.zip(distances).map(race => Race(race._1.toLong, race._2.toLong)).toList
  }
}

case class Race(time: Long, distance: Long) {
  def marginOfError(): Long = {
    val lower = (0L to time).takeWhile(startTime => startTime * (time - startTime) <= distance).size
    time - (lower * 2) + 1
  }

}