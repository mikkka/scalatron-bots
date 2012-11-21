import WeightStrategies._
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
  def donttouchit(el: Element) = el match {
    case Snorg => true
    case Toxifera => true
    case Wall => true
    case MiniBot => true
    case EnemyBot => true
    case EnemyMiniBot => true
    case _ => false
  }

  def weightPos(view: View, pos: XY, vector: XY, weights: ElementXY => Double) =
    view.offsets(pos + vector, _ != Unknown).foldLeft(0.0) {(accu, exy) =>
      val cos = vector.scalar(exy.xy) / (vector.length * exy.xy.length)
      accu +  weights(exy) * (if (cos > 0.8) cos else 0.8)
    }

  def makeMove(input: Input, output: Output, weights: ElementXY => Double) = {
    val view = input.view

    val movesWeights = XY.directions.
      filter(xy => !donttouchit(view.from(xy))).
      map(xy => (xy, weightPos(view, view.center, xy, weights)))

    if (!movesWeights.isEmpty) {
      val currCoord = input.coords.head
      val lastCoords = input.coords

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

  //начинаем размножаться если энергии больше 250 и если концентрация наших ботов не слишком велика
  def makeLove(input: Input, output: Output) = {
    val view = input.view

    val emptyCount = input.view.linear(el => el != Wall && el != Unknown).size
    val botsCount = input.view.linear(el => el == MiniBot).size

    if (input.energy > 250 && (1.0 * botsCount) / emptyCount < 0.025) {
      val directions = XY.directions.
        filter(xy => !donttouchit(view.from(xy)))
      if (!directions.isEmpty)
        output.spawn(directions.head,
          "type" -> "swarm",
          //каждый десятый - хыщник!
          "mood" -> (if (math.random > 0.9) "aggressive" else "def"))
      else
        output
    } else {
      output
    }
  }

  def explode(input: Input, output: Output) = {
    val view = input.view
    val nearEnemies = view.
      offsets(view.center, {el => el == EnemyMiniBot || el == EnemyBot || el == Snorg}).
      filter(_.xy.length <= 3)

    val enemies3 = nearEnemies.size
    val enemies2 = nearEnemies.filter(_.xy.length <= 2).size
    val enemies1 = nearEnemies.filter(_.xy.length <= 1).size

    val explodeRadius =
      if (enemies3 > 0) {
        if (enemies3 >= 3 && (enemies2 == 0 || enemies1 == 0)) 3
        else if (enemies2 > 0 && enemies3 / enemies2 >= 3) 3
        else if (enemies1 > 0 && enemies2 / enemies1 >= 3) 2
        else if (enemies1 > 0) 1
        else 0
      } else 0

    if (explodeRadius > 0) output.explode(explodeRadius)
    else output
  }

  def eatRunLove(input: Input): Output = {
    makeLove(input, makeMove(input, new Output, eatAndRunWeights))
  }

  def aggressive(input: Input): Output = {
    explode(input, makeLove(input, makeMove(input, new Output, attackWeights)))
  }

  def goHome(input: Input): Output = {
    makeLove(input, makeMove(input, new Output, goHomeWeights))
  }

  def react(input: Input): Output = {
    if (input.generation == 0) eatRunLove(input)
    else if (input.energy > 1000) goHome(input)
    else if (input.inputOrElse("mood", "def") == "aggressive") aggressive(input)
    else eatRunLove(input)
  }
}