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
