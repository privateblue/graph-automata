import scala.collection.immutable.BitSet

opaque type Configuration = Int

object Configuration:
  def apply(n: Int): Configuration =
    require(n >= 0, "Must be non-zero")
    n

opaque type RuleSet = BitSet

object RuleSet:
  def apply(n: BigInt): RuleSet =
    0.until(n.bitLength).foldLeft(BitSet.empty)((s, i) => if n.testBit(i) then s + i else s)

extension (rules: RuleSet)
  def isActive(c: Configuration): Boolean =
    rules.contains(c)
