import WeightFunc._

/**
 * User: mick
 * Date: 21.11.12
 * Time: 16:55
 */
object WeightStrategies {
  def eatAndRunWeights(elxy: ElementXY) = elxy.el match {
    case Snorg => sqrWeight(-400.0, elxy.xy.length, 1, 100)
    case Toxifera => sqrtWeight(-10.0, elxy.xy.length, 0.1, 100)
    case Empty => sqrtWeight(1.0, elxy.xy.length, 0.1, 100)
    case Wall => sqrWeight(-10.0, elxy.xy.length, 0.1, 10000)
    case Fluppet => linearWeight(400.0, elxy.xy.length, 0.1, 100)
    case Zugar => linearWeight(200.0, elxy.xy.length, 0.1, 100)
    case _ => 0.0
  }

  def goHomeWeights(elxy: ElementXY) = elxy.el match {
    case Snorg => sqrWeight(-400.0, elxy.xy.length, 1, 100)
    case Toxifera => sqrtWeight(-10.0, elxy.xy.length, 0.1, 100)
    case Empty => sqrtWeight(1.0, elxy.xy.length, 0.1, 100)
    case Wall => sqrWeight(-10.0, elxy.xy.length, 0.1, 10000)
    case Fluppet => linearWeight(400.0, elxy.xy.length, 0.1, 100)
    case Zugar => linearWeight(200.0, elxy.xy.length, 0.1, 100)
    case Bot => sqrtWeight(1000.0, elxy.xy.length, 0.1, 100)
    case _ => 0.0
  }

  def attackWeights(elxy: ElementXY) = elxy.el match {
    case Snorg => sqrWeight(-400.0, elxy.xy.length, 1, 100)
    case Toxifera => sqrtWeight(-10.0, elxy.xy.length, 0.1, 100)
    case Empty => sqrtWeight(1.0, elxy.xy.length, 0.1, 100)
    case Wall => sqrWeight(-10.0, elxy.xy.length, 0.1, 10000)
    case Fluppet => linearWeight(400.0, elxy.xy.length, 0.1, 100)
    case Zugar => linearWeight(200.0, elxy.xy.length, 0.1, 100)
    case EnemyMiniBot => sqrtWeight(500, elxy.xy.length, 0, 1)
    case EnemyBot => sqrtWeight(500, elxy.xy.length, 0, 1)
    case _ => 0.0
  }
}
