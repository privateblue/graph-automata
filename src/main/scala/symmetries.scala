package symmetries

def dihedral(degree: Int): Set[Permutation] =
  val cycle = Cycle(0.until(degree): _*)
  Permutation
    .identity(degree)
    .permutations
    .filter((cycle.lists ++ cycle.reverse.lists).contains)
    .map(Permutation.apply)
