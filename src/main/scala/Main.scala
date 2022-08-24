@main def cw(): Unit = {
  println(Square(3, Set(0, 1, 2, 3, 6, 7)))
  println(Square.all(3).size)

  println(Permutation(0, 1, 2, 3).toCN)

  println(Permutation(0, 1, 3, 2) * Permutation(3, 2, 1, 0))

  println(dihedral(4))
}
