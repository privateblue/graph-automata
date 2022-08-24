val LN2: Double = math.log(2)

extension (v: BigInt)
  def log2: Double =
    if v.signum < 0 then Double.NaN
    else if v.signum < 1 then Double.NegativeInfinity
    else
      val l = v.bitLength - 977
      val nv = if l > 0 then v >> l else v
      val ln = math.log(nv.toDouble)
      if l > 0 then (ln + l.toDouble * math.log(2)) / LN2 else ln / LN2
