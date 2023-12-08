import scala.io.Source

object Day8 {
  val nodeRegex = "([A-Z]+) = \\(([A-Z]+), ([A-Z]+)\\)".r

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day8.txt").getLines().toList

    val (instructions, nodes) = parse(input)
    println(part1(instructions, nodes))
    println(part2(instructions, nodes))
  }

  def part2(instructions: List[Char], nodes: Map[String, (String, String)]): BigInt = {
    val startNodes = nodes.keys.filter(_.endsWith("A"))
    val endNodes = nodes.keys.filter(_.endsWith("Z")).toSet

    lcm(startNodes
      .map(startNode => BigInt(findStepCount(startNode, instructions, nodes, endNodes)))
      .toList)
  }

  def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(list: Seq[BigInt]): BigInt = list.foldLeft(BigInt(1))((a, b) => (a / gcd(a, b)) * b)

  def part1(instructions: List[Char], nodes: Map[String, (String, String)]): Int =
    findStepCount("AAA", instructions, nodes, Set("ZZZ"))

  private def findStepCount(startNode: String,
                            instructions: List[Char],
                            nodes: Map[String, (String, String)],
                            endNodes: Set[String]
                           ) = {
    LazyList.from(0).scanLeft(startNode) { (node, index) =>
      val instruction = instructions(index % instructions.size)
      val nextNode = instruction match {
        case 'L' => nodes(node)._1
        case 'R' => nodes(node)._2
      }

      nextNode
    }.zipWithIndex.find(n => endNodes.contains(n._1)).get._2
  }

  def parse(input: List[String]): (List[Char], Map[String, (String, String)]) = {
    val instructions = input.head

    val nodes = input.drop(2).map(node => {
      val groups = nodeRegex.findFirstMatchIn(node).get

      (groups.group(1), (groups.group(2), groups.group(3)))
    }).toMap

    (instructions.toList, nodes)
  }
}
