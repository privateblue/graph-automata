import scala.annotation.tailrec
import scala.collection.immutable.BitSet

def bigIntToBitSet(n: BigInt): BitSet =
  @tailrec
  def bigIntToBitSet0(n: BigInt, c: Int, s: BitSet): BitSet =
    val l = n.lowestSetBit
    if l == -1 then s
    else bigIntToBitSet0(n >> (l + 1), c + l + 1, s + (c + l))
  bigIntToBitSet0(n, 0, BitSet.empty)
def bitSetToBigInt(s: BitSet): BigInt =
  s.foldLeft(BigInt(0))((n, i) => n.setBit(i))
def vectorToBitSet(v: Vector[Boolean]): BitSet =
  v.indices.foldLeft(BitSet.empty)((s, i) => if v(i) then s + i else s)

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
    def check(c: Configuration, rs: RuleSet): Boolean =
      val isActive = symmetries.map(p => rs.isActive(c.permuteBy(p)))
      isActive.forall(identity) || isActive.forall(!_)

    val cs = math.log(size) / math.log(2)
    require(cs.isWhole, "Size must be a power of 2")
    val configSize = cs.toInt
    require(symmetries.forall(_.size == configSize), s"All symmetries must be of size $configSize")

    allOfSize(size)
      .filter { rs =>
        Configuration.allOfSize(configSize).map(check(_, rs)).forall(identity)
      }

extension (rules: RuleSet)
  def isActive(c: Configuration): Boolean =
    // TODO because the below conversion to Int, Configurations can only be Int's in practice. but then storing them
    //  as such would probably be more performant. it's just that the BigInt <-> BitSet conversion is so much easier
    //  to implement
    rules.contains(bitSetToBigInt(c).intValue)
