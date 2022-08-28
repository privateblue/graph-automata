package automata

import graph._

import scala.collection.immutable.BitSet

class Automaton(
    graph: Graph,
    rules: RuleSet,
    actives: BitSet,
    val history: Vector[BitSet]
):
  def step(n: Int): Automaton =
    if n <= 0 then this
    else
      Automaton(
        graph,
        rules,
        graph.vertices.foldLeft(BitSet.empty)((s, v) => if rules.isActive(configurationOf(v)) then s + v else s),
        history :+ actives
      ).step(n - 1)

  def configurationOf(v: Int): Configuration =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    Configuration((v +: graph.neighboursOf(v)).map(isActive).toVector)

  def isActive(v: Int): Boolean =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    actives.contains(v)

  override def toString: String =
    graph.vertices.map(v => if isActive(v) then "1" else "0").mkString

object Automaton:
  def empty(graph: Graph, rules: RuleSet): Automaton =
    init(graph, rules, BitSet.empty)

  def init(graph: Graph, rules: RuleSet, initialState: BitSet): Automaton =
    Automaton(graph, rules, initialState, Vector.empty)
