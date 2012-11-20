/**
 * User: mick
 * Date: 20.11.12
 * Time: 11:34
 */
object WeightFunc {
  def sqrWeight(weight: Double, length: Double, crit: Double, critMulti: Double) =
    if (length < crit)
      weight * critMulti
    else
      weight / (length * length)

  def sqrtWeight(weight: Double, length: Double, crit: Double, critMulti: Double) =
    if (length < crit)
      weight * critMulti
    else
      weight / math.sqrt(length)

  def linearWeight(weight: Double, length: Double, crit: Double, critMulti: Double) =
    if (length < crit) weight * critMulti
    else weight / length
}
