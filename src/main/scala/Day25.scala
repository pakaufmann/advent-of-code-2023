import scala.collection.mutable
import scala.io.Source

object Day25 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day25.txt").getLines().toList
    println(part1(parse(input)))
  }

  def part1(connections: Map[String, Set[String]]): Int = {
    val (start, end) = findFarthestAway(connections)

    val split = Iterator
      .iterate(connections) { c => c.removeConnections(c.findFastest(start, end)) }
      .dropWhile(_.findGroups()._2.isEmpty)
      .next()

    val (f, s) = split.findGroups()
    f.size * s.size
  }

  implicit class ConnectionOps(map: Map[String, Set[String]]) {
    def removeConnection(from: String, to: String): Map[String, Set[String]] =
      map.updatedWith(from) { cons =>
        Some(cons.getOrElse(Set()) - to)
      }

    def removeConnections(connections: List[String]): Map[String, Set[String]] = {
      connections.sliding(2).foldLeft(map) { case (cons, f :: t :: Nil) =>
        cons.removeConnection(f, t).removeConnection(t, f)
      }
    }

    def findFastest(from: String, to: String): List[String] = {
      val queue = mutable.Queue((from, List[String]()))
      val visited = mutable.Set[String]()

      while (queue.nonEmpty) {
        val (component, steps) = queue.dequeue()

        if (component == to) {
          return steps :+ component
        }

        if (!visited.contains(component)) {
          visited.add(component)
          queue.addAll(map(component).map(c => (c, steps :+ component)))
        }
      }

      List()
    }

    def findGroups(): (Set[String], Set[String]) = {
      val queue = mutable.Queue(map.head._1)
      val visited = mutable.Set[String]()

      while (queue.nonEmpty) {
        val component = queue.dequeue()

        if (!visited.contains(component)) {
          visited.add(component)
          queue.addAll(map(component))
        }
      }

      (visited.toSet, map.keySet -- visited)
    }
  }

  def findFarthestAway(connections: Map[String, Set[String]]): (String, String) = {
    val start = connections.head._1
    val queue = mutable.Queue(start)
    val visited = mutable.Set[String]()
    var end = ""

    while (queue.nonEmpty) {
      val component = queue.dequeue()

      if (!visited.contains(component)) {
        visited.add(component)
        queue.addAll(connections(component))
        end = component
      }
    }

    (start, end)
  }

  def parse(input: List[String]): Map[String, Set[String]] = {
    input.foldLeft(Map[String, Set[String]]()) { (existing, line) =>
      val name :: cons :: Nil = line.split(": ").toList
      val connections = cons.split(" ")

      val updated = existing.updatedWith(name) { cons =>
        Some(cons.getOrElse(Set()) ++ connections)
      }

      connections.foldLeft(updated) { (map, con) =>
        map.updatedWith(con) { cons =>
          Some(cons.getOrElse(Set()) + name)
        }
      }
    }
  }
}
