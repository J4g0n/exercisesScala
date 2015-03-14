package chapitre6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    if (int == Integer.MIN_VALUE) positiveInt(rng2)
    else (int.abs, rng2)
  }

  def positiveDouble(rng: RNG): (Double, RNG) = {
    val (int, rng2) = rng.nextInt
    if (int == Integer.MAX_VALUE) positiveDouble(rng2)
    else (int.abs.toDouble, rng2)
  }
}