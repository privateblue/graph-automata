def dihedral(degree: Int): Set[Permutation] =
  val cycle = Cycle(0.until(degree): _*)
  Permutation
    .ofSize(degree)
    .permutations
    .filter((cycle.lists ++ cycle.reverse.lists).contains)
    .map(Permutation.apply)
