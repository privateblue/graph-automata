package symmetries

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.BitSet

opaque type Permutation = Vector[Int]

object Permutation:
  def apply(is: Int*): Permutation =
    val p = is.toVector
    require(identity(p.size) == p.sorted, "Not a well-formed permutation")
    p

  def identity(size: Int): Permutation =
    0.until(size).toVector
end Permutation

extension (p: Permutation)
  infix def *(that: Permutation): Permutation =
    // TODO this requirement could be loosened to "at least of same size"
    require(p.size == that.size, "Permutations must be of same size")
    p.map(that.apply)

  def apply[A](v: Vector[A]): Vector[A] =
    // TODO this requirement could be loosened to "at least of same size"
    require(v.size == p.size, s"Must be of size ${p.size}")
    p.map(v.apply)

  def apply(s: BitSet): BitSet =
    s.map(p.apply)

  def size: Int =
    p.size

  def leftPad: Permutation =
    0 +: p.map(_ + 1)

  def permutations: Set[Permutation] =
    p.permutations.map(i => Permutation(i.toSeq: _*)).toSet

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
