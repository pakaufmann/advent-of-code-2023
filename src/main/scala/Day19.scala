import scala.collection.mutable
import scala.io.Source

object Day19 {
  private val workflowRegex = """([a-z]+)\{(.*)\}""".r
  private val comparisonRegex = "([a-z])([<>])([0-9]+)".r
  private val partRegex = """\{x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)\}""".r

  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day19.txt").getLines().toList

    val (workflows, parts) = parse(input)
    println(part1(workflows, parts))
    println(part2(workflows))
  }

  def part1(workflows: List[Workflow], parts: List[Part]): Long = {
    val workflowMap = workflows.map(w => (w.name, w)).toMap
    parts.filter(isValid(workflowMap)).map(_.rating).sum
  }

  def part2(workflows: List[Workflow]): Long = {
    val start = workflows.find(_.name == "in").get
    val queue = mutable.Queue((start, Ranges()))
    val accepted = mutable.Buffer[Ranges]()

    while (queue.nonEmpty) {
      val (workflow, ranges) = queue.dequeue()

      var lastRanges = ranges

      for (rule <- workflow.rules) {
        val (updatesRanges, invertedRanges) = rule.constraint match {
          case EmptyConstraint => (lastRanges, lastRanges)
          case c@VariableConstraint(_, _, _) => lastRanges.update(c)
        }
        if (rule.next == "A") {
          accepted.append(updatesRanges)
        } else if (rule.next != "R") {
          queue.addOne((workflows.find(_.name == rule.next).get, updatesRanges))
        }
        lastRanges = invertedRanges
      }
    }

    accepted.map(_.combinations()).sum
  }

  def isValid(workflowMap: Map[String, Workflow])(part: Part): Boolean = {
    var currentWorkflow = workflowMap("in")

    while (true) {
      val rule = currentWorkflow.rules.find(_.constraint.check(part)).get
      rule.next match {
        case "A" => return true
        case "R" => return false
        case _ => currentWorkflow = workflowMap(rule.next)
      }
    }

    false
  }

  def parse(input: List[String]): (List[Workflow], List[Part]) = {
    val (workflowList, partList) = input.splitAt(input.indexOf(""))

    val workflows = workflowList.map(workflow => {
      val res = workflowRegex.findAllIn(workflow)
      val rules = res.group(2).split(",").map(rule => {
        val split = rule.split(":")
        if (split.size == 1) {
          Rule(EmptyConstraint, split(0))
        } else {
          val comparison = comparisonRegex.findAllIn(split(0))
          val against = comparison.group(3).toInt

          Rule(
            VariableConstraint(
              comparison.group(1).head,
              comparison.group(2) == ">",
              against
            ),
            split(1))
        }
      })

      Workflow(res.group(1), rules.toList)
    })

    (
      workflows,
      partList.drop(1).map(part => {
        val parts = partRegex.findAllIn(part)
        Part(parts.group(1).toInt, parts.group(2).toInt, parts.group(3).toInt, parts.group(4).toInt)
      })
    )
  }
}

sealed trait Constraint {
  def check(part: Part): Boolean
}

case class VariableConstraint(variable: Char, greater: Boolean, num: Long) extends Constraint {
  override def check(part: Part): Boolean = {
    val toCheck = variable match {
      case 'x' => part.x
      case 'm' => part.m
      case 'a' => part.a
      case 's' => part.s
    }

    if (greater) toCheck > num else toCheck < num
  }
}

case object EmptyConstraint extends Constraint {
  override def check(part: Part): Boolean = true
}

case class Rule(constraint: Constraint, next: String)

case class Workflow(name: String, rules: List[Rule])

case class Part(x: Long, m: Long, a: Long, s: Long) {
  val rating: Long = x + m + a + s
}

case class Ranges(ranges: Map[Char, (Long, Long)] = Map(
  'x' -> (1, 4000),
  'm' -> (1, 4000),
  'a' -> (1, 4000),
  's' -> (1, 4000)
)) {
  def combinations(): Long = ranges.values.map(a => a._2 - a._1 + 1).product

  def update(constraint: VariableConstraint): (Ranges, Ranges) = {
    (Ranges(ranges.updatedWith(constraint.variable) { rangeOpt =>
      val (start, end) = rangeOpt.get

      if (constraint.greater) {
        Some((constraint.num + 1) -> end)
      } else {
        Some(start -> (constraint.num - 1))
      }
    }),
      Ranges(ranges.updatedWith(constraint.variable) { rangeOpt =>
        val (start, end) = rangeOpt.get

        if (!constraint.greater) {
          Some(constraint.num -> end)
        } else {
          Some(start -> constraint.num)
        }
      }))
  }
}