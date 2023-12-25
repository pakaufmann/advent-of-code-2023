import scala.collection.mutable
import scala.io.Source

object Day25 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day25.txt").getLines().toList
    println(part1(parse(input)))
  }

  def part1(connections: Map[String, Set[String]]): Int = {
    val cons = connections.toList
      .flatMap { case (name, cons) => cons.map(c => (if (name.compareTo(c) < 0) c else name, if (name.compareTo(c) < 0) name else c)) }
      .toSet

    val (start, end) = findFarthestAway(connections)

    val first = findFastest(start, end, connections)
    val remaining = first.sliding(2).foldLeft(connections) { case (cons, f :: t :: Nil) =>
      cons.removeConnection(f, t).removeConnection(t, f)
    }

    val second = findFastest(start, end, remaining)
    val remaining2 = second.sliding(2).foldLeft(remaining) { case (cons, f :: t :: Nil) =>
      cons.removeConnection(f, t).removeConnection(t, f)
    }

    val third = findFastest(start, end, remaining2)

    for (f1 :: t1 :: Nil <- first.sliding(2)) {
      for (f2 :: t2 :: Nil <- second.sliding(2)) {
        for (f3 :: t3 :: Nil <- third.sliding(2)) {
          val removed = connections.removeConnection(f1, t1)
            .removeConnection(t1, f1)
            .removeConnection(f2, t2)
            .removeConnection(t2, f2)
            .removeConnection(f3, t3)
            .removeConnection(t3, f3)
          val (first, second) = findGroups(removed)

          if (second.nonEmpty) {
            return first.size * second.size
          }
        }
      }
    }

    0
  }

  implicit class ConnectionOps(connections: Map[String, Set[String]]) {
    def removeConnection(from: String, to: String): Map[String, Set[String]] =
      connections.updatedWith(from) { cons =>
        Some(cons.getOrElse(Set()) - to)
      }
  }

  def findFastest(from: String, to: String, connections: Map[String, Set[String]]): List[String] = {
    val queue = mutable.Queue((from, List[String]()))
    val visited = mutable.Set[String]()

    while (queue.nonEmpty) {
      val (component, steps) = queue.dequeue()

      if (component == to) {
        return steps :+ component
      }

      if (!visited.contains(component)) {
        visited.add(component)
        queue.addAll(connections(component).map(c => (c, steps :+ component)))
      }
    }

    List()
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

  def findGroups(connections: Map[String, Set[String]]): (Set[String], Set[String]) = {
    val queue = mutable.Queue(connections.head._1)
    val visited = mutable.Set[String]()

    while (queue.nonEmpty) {
      val component = queue.dequeue()

      if (!visited.contains(component)) {
        visited.add(component)
        queue.addAll(connections(component))
      }
    }

    (visited.toSet, connections.keySet -- visited)
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
