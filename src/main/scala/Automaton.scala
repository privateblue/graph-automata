import scala.collection.immutable.BitSet

class Automaton(
    graph: Graph,
    actives: BitSet
):
  // TODO find better syntax than applying to rule set
  // TODO keep entire history
  def apply(rules: RuleSet): Automaton =
    Automaton(
      graph,
      graph.vertices.foldLeft(BitSet.empty)((s, v) => if rules.isActive(configurationOf(v)) then s + v else s)
    )

  def configurationOf(v: Int): Configuration =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    Configuration((v +: graph.neighboursOf(v)).map(isActive).toVector)

  def isActive(v: Int): Boolean =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    actives.contains(v)

  override def toString: String =
    graph.vertices.map(v => if isActive(v) then "1" else "0").mkString

object Automaton:
  def empty(graph: Graph): Automaton =
    Automaton(graph, BitSet.empty)
