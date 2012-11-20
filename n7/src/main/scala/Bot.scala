import WeightFunc._
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
      val output = BotStrategies.react(input)
      output.toString
    } else ""
  }
}

object BotStrategies {
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

  def eatAndRun(input: Input, output: Output) = {
    val view = input.view

    val movesWeights = XY.directions.
      filter(xy => !donttouchit(view.from(xy))).
      map(xy => (xy, weightPos(view, view.center, xy)))

    if (!movesWeights.isEmpty) {
      val currCoord = input.coords.head
      val lastCoords = input.coords.take(10)

      val move = movesWeights.
        //поправка на предыдущие ходы
        map(mw =>
        if (lastCoords.contains(currCoord + mw._1))
          (mw._1, mw._2 - 50)
        else
          mw
      ).maxBy(_._2)._1

      new Output(Map.empty, input.history).move(move)
    } else {
      output
    }
  }

  def makeLove(input: Input, output: Output) = {
    val view = input.view
    val dangerDirections = XY.directions.map(xy =>
      (xy, view.part45(xy).count(elxy => elxy.el == Snorg && (3 to 6).contains(elxy.xy.length)))
    ).filter((el) => el._2 > 1)

    val slaveCount = input.inputAsIntOrElse("slaveCount", 0)
    if (input.energy > 500 && !dangerDirections.isEmpty && slaveCount < 4 && input.generation < 4) {
      val dangerDirection = dangerDirections.maxBy(_._2)._1
      output.spawn(dangerDirection, "type" -> "swarm").set("slaveCount" -> (slaveCount + 1))
    } else {
      output
    }
  }

  def eatRunLove(input: Input): Output = {
    makeLove(input, eatAndRun(input, new Output))
  }

  def goHome(input: Input): Output = {
    new Output()
  }

  def react(input: Input): Output = {
    if (input.generation == 0 || input.energy < 1000) eatRunLove(input)
    else goHome(input)
  }
}