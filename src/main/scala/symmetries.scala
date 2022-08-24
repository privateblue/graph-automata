def dihedral(n: Int): List[Vector[Int]] =
  val cycle = Cycle(0.until(n): _*)
  Permutation.ofSize(n).permutations.filter((cycle.lists ++ cycle.reverse.lists).contains)
