import scala.collection.immutable.BitSet

val x = BigInt(2)
x.log2
x.bitLength

val ruleSet = 0.until(x.bitLength).map(x.testBit).toVector

val config = Vector(true, false, true)

val b = BitSet.empty

BitSet(4) + 3

// ---

val g = Graph.empty.add.add.add.add
  .connect(0, Seq(1, 2))
  .connect(1, Seq(3))
  .connect(2, Seq(3))

val au = Automaton(g, BitSet(1, 2, 3))

au.configurationOf(0)
au.configurationOf(1)
au.configurationOf(2)
au.configurationOf(3)

// 000 -> 0
// 001 -> 0
// 010 -> 1
// 011 -> 1
// 100 -> 0
// 101 -> 0
// 110 -> 0
// 111 -> 1
// 2,3,7
// 10001100 = 4+8+128 = 140

val rules = RuleSet(140)

// 01
// 11
//
// 00
// 01
//
// 00
// 00
// ...

au(rules)
au(rules)(rules)
au(rules)(rules)(rules)

val au2 = Automaton(g, BitSet(1, 3))

// 01
// 01
//
// 10
// 01
//
// 00
// 00
// ...

au2(rules)
au2(rules)(rules)
