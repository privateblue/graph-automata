import symmetries._
import automata._

@main def main(): Unit =
  // 3-neighbour configurations
  // (2^(3+1) = 16 configurations per rule set, and 2^2^(3+1) = 65536 possible rule sets)
  val symmetrical3neighbourRuleSets = RuleSet.allWithSymmetries(16, dihedral(3).map(_.leftPad))
  println(symmetrical3neighbourRuleSets.size)
  symmetrical3neighbourRuleSets.foreach(println)
