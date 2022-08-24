import scala.annotation.{tailrec, targetName}

opaque type Permutation = Vector[Int]

object Permutation:
  def apply(is: Int*): Permutation =
    val p = is.toVector
    require(ofSize(p.size) == p.sorted, "Not a well-formed permutation")
    p

  def ofSize(size: Int): Permutation =
    0.until(size).toVector
end Permutation

extension (p: Permutation)
  @targetName("combine")
  infix def *(that: Permutation): Permutation =
    require(p.size == that.size, "Permutations must be of same size")
    p.map(that(_))

  def permutations: List[Permutation] =
    p.permutations.map(i => Permutation(i.toSeq: _*)).toList

  def toCN: CN =
    @tailrec
    def toCN0(start: Int, v: Vector[Int]): Vector[Int] =
      if v.isEmpty then toCN0(start, v :+ start)
      else
        val next = p(v.last)
        if next == v.head then v else toCN0(start, v :+ next)
    @tailrec
    def toCN1(cycles: List[Vector[Int]]): CN =
      val missing = p.filterNot(cycles.flatMap(identity).contains)
      if missing.isEmpty then cycles.map(Cycle(_: _*)) else toCN1(cycles :+ toCN0(missing.min, Vector.empty))
    toCN1(List.empty)
