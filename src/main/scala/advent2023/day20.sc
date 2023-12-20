val input = scala.io.Source.fromResource(s"advent2023/day20.txt").getLines().toSeq

enum Pulse:
  case Low
  case High
import Pulse.*

trait Output:
  def next: Seq[String]

enum Module extends Output:
  case Broadcaster(next: Seq[String])
  case FlipFlop(next: Seq[String], state: Boolean)
  case Conjunction(next: Seq[String], inputState: Map[String, Pulse])

import Module.*

val initModules = input.map {
  case s"broadcaster -> ${nextModules}" => "broadcaster" -> Broadcaster(nextModules.split(", "))
  case s"%${module} -> ${nextModules}"  => module        -> FlipFlop(nextModules.split(", "), false)
  case s"&${module} -> ${nextModules}"  => module        -> Conjunction(nextModules.split(", "), Map.empty)
}.toMap

val originalState = initModules
  .map {
    case name -> Conjunction(next, state) =>
      name -> Conjunction(next, initModules.filter(_._2.next.exists(_.contains(name))).keys.map(_ -> Low).toMap)
    case other                            => other
  }

case class Signal(from: String, to: String, pulse: Pulse):
  override def toString = s"$from --${if pulse == High then "high" else "low"}-> $to"

case class Counter(low: Int, high: Int, pressCount: Int, cycleLength: Map[String, Int]):
  def product                     = low * high * 1L
  def add(pulse: Pulse)           = if pulse == High then this.copy(high = high + 1) else this.copy(low = low + 1)
  def pressButton                 = this.copy(pressCount = pressCount + 1)
  def recordPulse(module: String) =
    if cycleLength.contains(module) then this
    else this.copy(cycleLength = cycleLength.updated(module, pressCount))

val rxOrigin = originalState.find { case (name, module) => module.next.contains("rx") }.head._1

def process(signals: Seq[Signal], state: Map[String, Module], tracker: Counter): (Map[String, Module], Counter) =
  if signals.isEmpty then (state, tracker)
  else
    val Signal(from, to, pulse) = signals.head

    val counter =
      if to == rxOrigin && pulse == High then tracker.add(pulse).recordPulse(from)
      else tracker.add(pulse)

    if !state.contains(to) then process(signals.tail, state, counter)
    else
      state(to) match
        case Broadcaster(next) =>
          val nextSignals = next.map(mod => Signal(to, mod, pulse))
          process(signals.tail ++ nextSignals, state, counter)

        case FlipFlop(next, isOn) if pulse == High =>
          process(signals.tail, state, counter)

        case FlipFlop(next, isOn) =>
          val nextSignals = next.map(mod => Signal(to, mod, if isOn then Low else High))
          process(signals.tail ++ nextSignals, state.updated(to, FlipFlop(next, !isOn)), counter)

        case Conjunction(next, inputState) =>
          val nextInputState = inputState.updated(from, pulse)
          val nextSignals    =
            if nextInputState.values.forall(_ == High) then next.map(mod => Signal(to, mod, Low))
            else next.map(mod => Signal(to, mod, High))
          process(signals.tail ++ nextSignals, state.updated(to, Conjunction(next, nextInputState)), counter)

val part1 = (1 to 1000)
  .foldLeft((originalState, Counter(0, 0, 0, Map.empty))) { case (state, counter) -> _ =>
    process(Seq(Signal("button", "broadcaster", Low)), state, counter.pressButton)
  }
  ._2
  .product

val part2 = (1 to 5000)
  .foldLeft((originalState, Counter(0, 0, 0, Map.empty))) { case (state, counter) -> _ =>
    process(Seq(Signal("button", "broadcaster", Low)), state, counter.pressButton)
  }
  ._2
  .cycleLength
  .values
  .map(_.toLong)
  .product // all cycles are primes, so just multiplying it instead of lcm
