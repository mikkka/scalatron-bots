import util.Random
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
      if (input.collision != XY.Zero)
        println("collision on " + input.mood + " " + input.collision)
      val output = BotStrategies.react(input)
      output.toString
    } else ""
  }
}

object BotStrategies {
  val rand = new Random()

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

  def makeMove(input: Input, weights: ElementXY => Double, directionWeights: ((XY, Double)) => (XY, Double)) = {
    val view = input.view

    val movesWeights = XY.directions.
      filter(xy => !donttouchit(view.from(xy))).
      map(xy => (xy, weightPos(view, view.center, xy, weights)))

    if (!movesWeights.isEmpty) {
      val move = movesWeights.map(mw => directionWeights(mw)).maxBy(_._2)._1

      Some(Move(move))
    } else {
      None
    }
  }

  def cycleInhibit(input: Input)(mw: (XY, Double)) = {
    val currCoord = input.coords.head
    val lastCoords = input.coords

    if (lastCoords.contains(currCoord + mw._1)) (mw._1, mw._2 - 50)
    else mw
  }

  def aggressiveGoHomeDir(input: Input)(mw: (XY, Double)) = {
    val dir = mw._1
    val weight = mw._2
    val offsetToMaster = input.offsetToMaster
    val cos = dir.scalar(offsetToMaster) / (dir.length * offsetToMaster.length)

    (dir, weight * (if (cos > 0.7) cos else 0.7))
  }

  //начинаем размножаться если энергии больше 250 и если концентрация наших ботов не слишком велика
  def makeLove(input: Input) = {
    val view = input.view
    val directions = XY.directions.
      filter(xy => !donttouchit(view.from(xy)))

    if (!directions.isEmpty) {
      val dir = if (directions.length == 1) directions(0) else directions(rand.nextInt(directions.length - 1))
      if(input.energy > 5000 && input.generation > 0) {
        Some(Spawn(dir, "feeder", 2500, "feeder"))
      } else {
        val emptyCount = input.view.linear(el => el != Wall && el != Unknown).size
        val friendlyBotsCount = input.view.linear(el => el == MiniBot).size

        if (input.energy > 250 && (1.0 * friendlyBotsCount) / emptyCount < 0.05) {
          val enemyBotsCount = input.view.linear(el => el == EnemyMiniBot).size

          val agrressiveCoeff = if (friendlyBotsCount * 0.6 > enemyBotsCount) 0.8
          else 0.7

          val energy = if (input.energy > 5000) 500
          else if (input.energy < 1000) 100
          else input.energy / 10
          if (rand.nextDouble() > agrressiveCoeff)
            Some(Spawn(dir, "shahid", energy, "shahid"))
          else
            Some(Spawn(dir, "hippie", energy, "hippie"))
        } else {
          None
        }
      }
    } else {
      None
    }
  }

  def explode(input: Input, offset: XY) = {
    val view = input.view
    val nearEnemies = view.
      offsets(view.center + offset, {el => el == EnemyBot || el == EnemyMiniBot || el == Snorg}).
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

    if (explodeRadius > 0) Some(Explode(explodeRadius))
    else None
  }

  def master(input: Input, output: Output): Output = {
    output.
      append(makeMove(input, eatAndRunWeightsMaster, cycleInhibit(input))).
      append(makeLove((input)))
  }

  def eatRunLove(input: Input, output: Output): Output = {
    output.
      append(makeMove(input, eatAndRunWeights, cycleInhibit(input))).
      append(makeLove(input))
  }

  def aggressive(input: Input, output: Output): Output = {
    val move = makeMove(input, attackWeights, cycleInhibit(input))
    val offset = move match {
      case Some(move) => move.offset
      case None => XY(0, 0)
    }
    val explodeCmd = explode(input, offset)
    explodeCmd match {
      case Some(explode) =>
        output.append(explode).say("BORODA!!!").append(move)
      case None =>
        output.append(move).append(makeLove(input))
    }
  }

  def goHome(input: Input, output: Output): Output = {
    output.
      append(makeMove(input, goHomeWeights, cycleInhibit(input))).
      append(makeLove(input))
  }

  def aggressiveGoHome(input: Input, output: Output): Output = {
    val cycleInhibitor = cycleInhibit(input)(_)
    val homeDir = aggressiveGoHomeDir(input)(_)
    val homeWithoutCycle = {x: (XY, Double) => homeDir(cycleInhibitor(x))}

    output.
      append(makeMove(input, goHomeWeights, homeWithoutCycle)).
      append(makeLove(input))
  }

  def react(input: Input): Output = {
    val out = new Output(Map.empty, input.history)
    val mood = input.inputOrElse("mood", "")
    val energy = input.energy
    val generation = input.generation

    if (generation == 0) master(input, out)
    else if (input.time > 4900) aggressiveGoHome(input, out)
    else if (input.time > 4999) out.append(Explode(3)).say("watafaka!")
    else if (energy > 1000) goHome(input, out)
    else if (mood == "shahid") aggressive(input, out)
    else if (mood == "feeder" && energy > 1000) aggressiveGoHome(input, out)
    else if (math.random > 0.999) aggressive(input, out).say("AAARGH!!!")
    else eatRunLove(input, out)
  }
}