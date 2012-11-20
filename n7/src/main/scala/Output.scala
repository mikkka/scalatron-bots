/**
 * User: mick
 * Date: 13.11.12
 * Time: 18:39
 */
case class Output(stateParams: Map[String, Any], commands: String, debugOutput: String,
                  history: List[(XY, XY)]) {
  val historySize = 50

  def this() = this(Map.empty, "", "", List.empty)
  def this(params: Map[String, String]) = this(params, "", "", List.empty)
  def this(params: Map[String, String], h: List[(XY, XY)]) = this(params, "", "", h)

  private def append(s: String) =
    new Output(stateParams, (if(commands.isEmpty) s else commands + "|" + s), debugOutput, history)

  def set(params: (String,Any)*) = new Output(stateParams ++ params, commands, debugOutput, history)

  def set(keyPrefix: String, xy: XY) =
    new Output(stateParams ++ List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y), commands, debugOutput, history)

  def log(text: String) = new Output(stateParams, commands, debugOutput + text + "\n", history)

  /*
    adding move to history. should automatically compute new coords
   */
  def addHistory(move: XY) = {
    val prevH = history.headOption.getOrElse((XY(0,0),XY(0,0)))
    val newh = (move, prevH._2 + move)
    new Output(stateParams, commands, debugOutput, (newh :: history).take(10))
  }

  override def toString = {
    var result = commands
    if(!stateParams.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }

    //write moves
    if(!history.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Set(_history=" + history.map(el => el._1.toString + ";" + el._2.toString).mkString(";") + ")"
    }

    if(!debugOutput.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def move(direction: XY) =
    addHistory(direction).append("Move(direction=" + direction + ")")

  def say(text: String) = append("Say(text=" + text + ")")
  def status(text: String) = append("Status(text=" + text + ")")
  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")

  def spawn(offset: XY, params: (String,Any)*) =
    append("Spawn(direction=" + offset +
      (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")
}
