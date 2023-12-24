import scala.io.Source

object Day24 {
  val hailstoneRegex = """(.+), (.+), (.+) @ (.+), (.+), (.+)""".r

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day24.txt").getLines().toList
    println(part1(parse(input), 200000000000000L, 400000000000000L))
  }

  def part1(hailstones: List[Hailstone], from: Long, to: Long): Int = {
    hailstones.combinations(2).count { case f :: s :: Nil =>
      f.intersect(s).exists(p => {
        val (x, y) = p
        val inBoundaries = x >= from && x <= to && y >= from && y <= to
        val inPastF = if (f.velocity.y < 0) y > f.position.y else y < f.position.y
        val inPastS = if (s.velocity.y < 0) y > s.position.y else y < s.position.y
        inBoundaries && !inPastF && !inPastS
      })
    }
  }

  def parse(input: List[String]): List[Hailstone] = {
    input.map { line =>
      val res = hailstoneRegex.findAllIn(line)
      Hailstone(
        LCoordinate3D(res.group(1).trim.toLong, res.group(2).trim.toLong, res.group(3).trim.toLong),
        LCoordinate3D(res.group(4).trim.toLong, res.group(5).trim.toLong, res.group(6).trim.toLong)
      )
    }
  }
}

case class LCoordinate3D(x: Long, y: Long, z: Long) {
}

case class Hailstone(position: LCoordinate3D, velocity: LCoordinate3D) {
  val gradient: BigDecimal = BigDecimal(velocity.y) / velocity.x
  val yOffset: BigDecimal = position.y - gradient * position.x
  val toLeft: Boolean = velocity.x < 0

  def intersect(other: Hailstone): Option[(BigDecimal, BigDecimal)] = {
    if (gradient == other.gradient) return None
    val atX = (yOffset - other.yOffset) / (other.gradient - gradient)
    Some((atX, linearFunc(atX)))
  }

  def linearFunc(x: BigDecimal): BigDecimal =
    gradient * x + yOffset
}
