import scala.collection.mutable
import scala.io.Source

object Day12 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day12.txt").getLines().toList

    println(count(parse(input)))
    println(count(parse(input).map(increase(5))))
  }

  def increase(by: Int)(arrangement: Arrangement): Arrangement = {
    if (by == 1) return arrangement
    val increased = increase(by - 1)(arrangement)
    Arrangement(
      (arrangement.springs :+ '?') ++ increased.springs,
      arrangement.toPlace ++ increased.toPlace
    )
  }

  def count(arrangements: List[Arrangement]): Long =
    arrangements
      .map(arrangement => countArrangement(arrangement.springs, arrangement.toPlace))
      .sum

  private val cache = mutable.Map[(List[Char], List[Int]), Long]()

  def countArrangement(remaining: List[Char], toPlace: List[Int]): Long = {
    val springs = remaining.dropWhile(_ == '.')

    cache.getOrElseUpdate((springs, toPlace), {
      if (toPlace.isEmpty) return if (springs.contains('#')) 0 else 1
      if (springs.isEmpty) return 0

      val nextToPlace :: remainingToPlace = toPlace.dropWhile(_ == '.')
      val nextSpring :: remainingSprings = springs
      val nextSprings = springs.take(nextToPlace)
      val canPlace = nextSprings.size == nextToPlace &&
        nextSprings.forall(c => c == '?' || c == '#') &&
        !springs.drop(nextToPlace).headOption.contains('#')

      (nextSpring, canPlace) match {
        case ('#', true) => countArrangement(springs.drop(nextToPlace + 1), remainingToPlace)
        case ('#', false) => 0
        case (_, true) => countArrangement(springs.drop(nextToPlace + 1), remainingToPlace) +
          countArrangement(remainingSprings, toPlace)
        case _ =>
          countArrangement(remainingSprings, toPlace)
      }
    })
  }

  def parse(input: List[String]): List[Arrangement] = {
    input.map(row => {
      val split = row.split(' ')

      Arrangement(
        split(0).toList,
        split(1).split(',').map(_.toInt).toList
      )
    })
  }
}

case class Arrangement(springs: List[Char], toPlace: List[Int])
