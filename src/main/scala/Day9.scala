import scala.collection.mutable
import scala.io.Source

object Day9 {

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day9.txt").getLines().toList

    val sequences = input.map(_.split(" ").map(_.toInt).toList)

    println(generate(sequences, generateNext))
    println(generate(sequences, generatePrev))
  }

  def generate(sequences: List[List[Int]], generate: Seq[List[Int]] => Int): Int = {
    sequences
      .map(sequence => generate(generateDiffs(sequence)))
      .sum
  }

  def generatePrev(sequences: Seq[List[Int]]): Int =
    sequences.foldRight(0) {
      case (sequence, first) => sequence.head - first
    }

  def generateNext(sequences: Seq[List[Int]]): Int =
    sequences.foldRight(0) {
      case (sequence, last) => sequence.last + last
    }

  def generateDiffs(sequence: List[Int]): Seq[List[Int]] =
    LazyList.iterate(sequence)(generateDiff)
      .takeWhile(!_.forall(_ == 0))

  def generateDiff(sequence: List[Int]): List[Int] =
    sequence
      .sliding(2)
      .map { case f :: l :: Nil => l - f }
      .toList
}
