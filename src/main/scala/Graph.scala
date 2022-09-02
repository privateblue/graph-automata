package graph

opaque type Graph = IndexedSeq[Seq[Int]]

object Graph:
  def empty: Graph =
    IndexedSeq.empty[Seq[Int]]

  def complete(size: Int): Graph =
    require(size >= 0, "Size must be non-negative")
    val range = 0.until(size)
    range.map(n => range.filterNot(_ == n))

  def path(size: Int): Graph =
    require(size >= 0, "Size must be non-negative")
    0.until(size).map(i => Seq(i - 1, i + 1).filter(n => n >= 0 && n < size))

  def cycle(size: Int): Graph =
    require(size >= 0, "Size must be non-negative")
    if size == 1 then path(1)
    else if size == 2 then path(2)
    else -1.until(size - 1).map(i => Seq(math.floorMod(i, size), math.floorMod(i + 2, size)))
end Graph

extension (graph: Graph)
  def size: Int =
    graph.size

  def vertices: Seq[Int] =
    0.until(size)

  def edges: Seq[(Int, Int)] =
    graph.vertices.flatMap(i => graph(i).map((i, _))).filter { case (f, t) => f < t }

  def neighboursOf(v: Int): Seq[Int] =
    require(v < graph.size, s"$v out of bounds (${graph.size - 1})")
    graph(v)

  def add(n: Int): Graph =
    graph ++ IndexedSeq.fill(n)(Seq.empty[Int])

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

  def subdivide(at: (Int, Int)): Graph =
    require(graph.edges.contains(at) || graph.edges.contains(at.swap), s"$at must be an edge of the graph")
    graph.disconnect(at._1, Seq(at._2)).add(1).connect(size, Seq(at._1, at._2))

  infix def U(that: Graph): Graph =
    that.foldLeft(graph)((g, ns) => g :+ ns.map(_ + graph.size))

  infix def X(that: Graph): Graph =
    for {
      thatV <- that.vertices.toIndexedSeq
      thatN = that(thatV)
      thisV <- graph.vertices
      thisN = graph(thisV)
    } yield thisN.map(thatV * graph.size + _) ++ thatN.map(_ * graph.size + thisV)

  def glue(that: Graph, at1: (Int, Int), at2: (Int, Int)): Graph =
    require(graph.edges.contains(at1), s"$at1 must be an edge of the left graph")
    require(that.edges.contains(at2), s"$at2 must be an edge of the right graph")
    val thisRemoved = graph.disconnect(at1._1, Seq(at1._2))
    val thatRemoved = that.disconnect(at2._1, Seq(at2._2))
    (thisRemoved U thatRemoved)
      .connect(at1._1, Seq(at2._1 + graph.size))
      .connect(at1._2, Seq(at2._2 + graph.size))
