/**
 * User: mick
 * Date: 29.08.12
 * Time: 17:54
 */
class ControlFunctionFactory {
  def create = ControlFunction.respond _
}

object ControlFunction {
  def respond(input: String): String = {
    val (opcode, paramMap) = CommandParser(input)

    if( opcode == "React" ) {
      val input = new Input(paramMap)
      val output = MainBot.react(input)
      output.toString
    } else ""
  }
}

object MainBot {
  def weigthElRelative(elxy: ElementXY) = elxy.el match {
    case Snorg => sqrWeight(-400.0, elxy.xy.length, 1, 100)
    case Toxifera => sqrtWeight(-10.0, elxy.xy.length, 0.1, 100)
    case Empty => sqrtWeight(1.0, elxy.xy.length, 0.1, 100)
    case Wall => sqrWeight(-10.0, elxy.xy.length, 0.1, 10000)
    case Fluppet => linearWeight(400.0, elxy.xy.length, 0.1, 100)
    case Zugar => linearWeight(200.0, elxy.xy.length, 0.1, 100)
    case _ => 0.0
  }

  def donttouchit(el: Element) = el match {
    case Snorg => true
    case Toxifera => true
    case Wall => true
    case _ => false
  }

  def weightPos(view: View, pos: XY, vector: XY) = view.offsets(pos + vector, _ != Unknown).foldLeft(0.0) {(accu, exy) =>
    val cos = vector.scalar(exy.xy) / (vector.length * exy.xy.length)
    accu +  weigthElRelative(exy) * (if (cos > 0.8) cos else 0.8)
  }

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

  def react(input: Input): Output = {
    val view = input.view

    val unitOffsets = (for (
      x <- -1 to 1;
      y <- -1 to 1
      if !(x == 0 && y == 0 && !donttouchit(view.relative(XY(x, y))))
    ) yield (XY(x,y), weightPos(view, view.center, XY(x,y))))

    if (!unitOffsets.isEmpty) {
      val move = unitOffsets.maxBy(_._2)._1
      new Output(input.params, input.history).move(move)
    } else {
      new Output
    }
  }
}
