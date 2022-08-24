import scala.collection.immutable.BitSet

class Automaton(
    graph: Graph,
    actives: BitSet
):
  def apply(rules: RuleSet): Automaton =
    Automaton(
      graph,
      graph.vertices.foldLeft(BitSet.empty)((s, v) => if rules.isActive(configurationOf(v)) then s + v else s)
    )

  def configurationOf(v: Int): Configuration =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    val conf = graph
      .neighboursOf(v)
      .zipWithIndex
      .foldLeft(if isActive(v) then 1 else 0) { case (c, (n, i)) =>
        if isActive(n) then c + math.pow(2, i + 1).toInt else c
      }
    Configuration(conf)

  def isActive(v: Int): Boolean =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    actives.contains(v)

  override def toString: String =
    graph.vertices.map(v => if isActive(v) then "1" else "0").mkString

object Automaton:
  def empty(graph: Graph): Automaton =
    Automaton(graph, BitSet.empty)
