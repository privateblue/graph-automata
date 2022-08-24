case class Square(size: Int, points: Set[Int]):
  override def toString =
    (0 until size * size)
      .map(i => if points.contains(i) then "X" else ".")
      .grouped(size)
      .map(_.mkString)
      .mkString("\n")

  def map(f: Int => Int) =
    Square(size, points.map(f))

  def add(i: Int): Square =
    Square(size, points + i)
end Square

object Square:
  def all(size: Int): Set[Square] =
    def all0(squares: Set[Set[Int]], c: Int): Set[Set[Int]] =
      if c == size * size then squares
      else squares ++ (c until size * size).toSet.flatMap(i => all0(squares.map(_ + i), c + 1))
    all0(Set(Set()), 0).map(Square(size, _))
end Square

object D4:
  def generatorA(p: Square): Square =
    p.map(i => (i % p.size) * p.size + p.size - (i / p.size) - 1)

  def generatorB(p: Square): Square =
    p.map(i => (i / p.size) * p.size + p.size - (i % p.size) - 1)

  val elements: Set[Square => Square] =
    val c1 = Set(
      identity[Square],
      generatorA,
      generatorA.compose(generatorA),
      generatorA.compose(generatorA).compose(generatorA)
    )
    val c2 = c1.map(_.compose(generatorB))
    c1 ++ c2
end D4
