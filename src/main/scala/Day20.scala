import Day8.{gcd, lcm}
import Pulses.{High, Low, Pulse}

import scala.collection.mutable
import scala.io.Source

object Day20 {
  def main(args: Array[String]) = {
    val input = Source.fromFile("inputs/day20.txt").getLines().toList

    println(part1(parse(input)))
    println(part2(parse(input)))
  }

  def part1(modules: List[Module]): Long = {
    var state = modules.map(m => m.name -> m).toMap
    var totalLow = 0L
    var totalHigh = 0L
    for (i <- 0 until 1000) {
      val (newState, low, high) = runRound(state)
      state = newState
      totalLow += low
      totalHigh += high
    }

    totalLow * totalHigh
  }

  private val toFind = mutable.Set[String]()
  private val foundAt = mutable.Buffer[Long]()

  def part2(modules: List[Module]): BigInt = {
    var state = modules.map(m => m.name -> m).toMap
    var buttonPressCount = 0

    val conjunctions = modules.collect { case a: Conjunction => a }
    val sink = modules.collect { case s: Sink => s }.head
    val conjunctionIntoSink = conjunctions.find(_.connections.contains(sink.name)).get
    toFind.addAll(conjunctions.filter(_.connections.contains(conjunctionIntoSink.name)).map(_.name))

    while (true) {
      buttonPressCount += 1
      val (newState, _, _) = runRound(state, buttonPressCount)
      state = newState

      if (toFind.isEmpty) {
        return lcm(foundAt.map(a => BigInt(a)).toSeq)
      }
    }

    0
  }

  def runRound(moduleMap: Map[String, Module], i: Long = 0): (Map[String, Module], Int, Int) = {
    val newState = mutable.Map.newBuilder[String, Module].addAll(moduleMap).result()

    val queue = mutable.Queue(("broadcaster", Low, "button"))
    var lowCount = 0
    var highCount = 0

    while (queue.nonEmpty) {
      val (name, pulse, from) = queue.dequeue()

      if (pulse == Low) {
        lowCount += 1
      } else {
        highCount += 1
      }

      newState(name) match {
        case Broadcaster(connections) =>
          queue.addAll(connections.map(c => (c, pulse, "broadcaster")))
        case f@FlipFlop(_, connections, state) =>
          if (pulse == Low) {
            newState.update(name, f.copy(state = !state))
            if (!state) {
              queue.addAll(connections.map(c => (c, High, name)))
            } else {
              queue.addAll(connections.map(c => (c, Low, name)))
            }
          }
        case c@Conjunction(_, inputs, connections) =>
          if (toFind.contains(name) && pulse == Low) {
            toFind.remove(name)
            foundAt.addOne(i)
          }
          val newInputs = inputs.updated(from, pulse)
          newState.update(name, c.copy(inputs = newInputs))
          if (newInputs.forall(_._2 == High)) {
            queue.addAll(connections.map(c => (c, Low, name)))
          } else {
            queue.addAll(connections.map(c => (c, High, name)))
          }
        case s@Sink(name, last) =>
          newState.update(name, s.copy(sent = if (last.contains(Low)) last else Some(pulse)))
      }
    }

    (newState.toMap, lowCount, highCount)
  }

  def parse(input: List[String]): List[Module] = {
    val modules = input.map[Module](line => {
      val name :: m :: Nil = line.split(" -> ").toList
      val modules = m.split(", ").map(_.trim).toList

      name match {
        case "broadcaster" => Broadcaster(modules)
        case s"%$name" => FlipFlop(name, modules, state = false)
        case s"&$name" => Conjunction(name, Map(), modules)
      }
    })

    val moduleMap = modules.map(m => m.name -> m).toMap

    val moduleToInputs = modules.flatMap(i => i.connections.map(_ -> i)).groupBy(_._1)

    val finalModules = mutable.Set[Module]()

    for ((name, inputs) <- moduleToInputs) {
      moduleMap.get(name) match {
        case Some(c@Conjunction(_, _, _)) =>
          finalModules.addOne(c.copy(inputs = inputs.map(i => i._2.name -> Low).toMap))
        case Some(module) =>
          finalModules.addOne(module)
        case None =>
          finalModules.addOne(Sink(name, None))
      }
    }

    finalModules.addAll(modules.filter(module => !finalModules.exists(_.name == module.name)))

    finalModules.toList
  }
}

object Pulses extends Enumeration {
  type Pulse = Value

  val High, Low = Value
}

sealed trait Module {
  val name: String
  val connections: List[String]
}

case class Broadcaster(connections: List[String]) extends Module {
  val name = "broadcaster"
}

case class FlipFlop(name: String, connections: List[String], state: Boolean) extends Module

case class Conjunction(name: String, inputs: Map[String, Pulse], connections: List[String]) extends Module

case class Sink(name: String, sent: Option[Pulse] = None) extends Module {
  override val connections: List[String] = List()
}