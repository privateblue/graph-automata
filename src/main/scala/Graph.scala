package graph

opaque type Graph = IndexedSeq[Seq[Int]]

object Graph:
  def empty: Graph =
    IndexedSeq.empty[Seq[Int]]

  def total(size: Int): Graph =
    val range = 0.until(size)
    range.map(n => range.filterNot(_ == n))
end Graph

extension (graph: Graph)
  def size: Int =
    graph.size

  def vertices: Seq[Int] =
    0.until(size)

  def edges: Seq[(Int, Int)] =
    graph.zipWithIndex.flatMap((es, i) => es.map((i, _))).filter { case (f, t) => f < t }

  def neighboursOf(v: Int): Seq[Int] =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    graph(v)

  def add: Graph =
    graph :+ Seq.empty

  def remove(v: Int): Graph =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    val removed = graph.take(v) ++ graph.drop(v + 1)
    removed.map(ns => ns.filterNot(_ == v).map(n => if n < v then n else n - 1))

  def connect(from: Int, to: Seq[Int]): Graph =
    require(!to.contains(from), "Cannot connect to itself")
    (to :+ from).foreach { i => require(i < graph.size, s"$i out of bounds (${graph.size - 1})") }
    val updated = graph.updated(from, graph(from) ++ to.distinct.filterNot(graph(from).contains))
    to.distinct.foldLeft(updated) { case (g, i) => g.updated(i, if g(i).contains(from) then g(i) else g(i) :+ from) }

  def disconnect(from: Int, to: Seq[Int]): Graph =
    (to :+ from).foreach { i => require(i < graph.size, s"$i out of bounds (${graph.size - 1})") }
    val updated = graph.updated(from, graph(from).filterNot(to.contains))
    to.foldLeft(updated) { case (g, i) => g.updated(i, g(i).filterNot(_ == from)) }
