import scala.annotation.tailrec

opaque type Cycle[A] = List[A]

object Cycle:
  def apply[A](as: A*): Cycle[A] =
    require(as.nonEmpty, "Cannot be empty")
    as.toList
end Cycle

extension [A](cycle: Cycle[A])
  def step: Cycle[A] =
    Cycle(cycle.tail :+ cycle.head: _*)

  def reverse: Cycle[A] =
    cycle.reverse

  def lists: List[List[A]] =
    @tailrec
    def lists0(c: Cycle[A], acc: List[List[A]]): List[List[A]] =
      if c.head == cycle.head then acc else lists0(c.step, c :: acc)
    lists0(step, List(cycle))

type CN = List[Cycle[Int]]
