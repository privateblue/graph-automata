package automata

import symmetries._

import scala.collection.immutable.BitSet

opaque type Configuration = BitSet

object Configuration:
  def apply(s: BitSet): Configuration =
    s

  def apply(n: BigInt): Configuration =
    bigIntToBitSet(n)

  def apply(v: Vector[Boolean]): Configuration =
    vectorToBitSet(v)

  def allOfSize(n: Int): Set[Configuration] =
    BitSet(0.until(n): _*).subsets().toSet

extension (c: Configuration)
  def permuteBy(p: Permutation): Configuration =
    p.apply(c)

opaque type RuleSet = BitSet

object RuleSet:
  def apply(n: BigInt): RuleSet =
    bigIntToBitSet(n)

  def allOfSize(n: Int): Set[RuleSet] =
    BitSet(0.until(n): _*).subsets().toSet
  def allWithSymmetries(size: Int, symmetries: Set[Permutation]): Set[RuleSet] =
    val cs = math.log(size) / math.log(2)
    require(cs.isWhole, "Size must be a power of 2")
    val configSize = cs.toInt
    require(symmetries.forall(_.size == configSize), s"All symmetries must be of size $configSize")
    allOfSize(size).filter(_.isSymmetrical(symmetries))

extension (rules: RuleSet)
  def isActive(c: Configuration): Boolean =
    // TODO due to the conversion below to .intValue, Configurations can only be Ints in practice. so representing them
    //  as such would probably be more performant. it's just that the BigInt <-> BitSet conversion is so much easier
    //  to implement...
    rules.contains(bitSetToBigInt(c).intValue)

  def isSymmetrical(symmetries: Set[Permutation]): Boolean =
    def check(c: Configuration): Boolean =
      val actives = symmetries.map(p => rules.isActive(c.permuteBy(p)))
      actives.size == 1
    require(!symmetries.isEmpty, "Must provide at least one symmetry")
    val size = symmetries.head.size
    require(symmetries.forall(_.size == size), "Symmetries must be of same size")
    require(symmetries.contains(Permutation.identity(size)), "Symmetries must include the identity symmetry")
    Configuration.allOfSize(size).map(check).forall(identity)
