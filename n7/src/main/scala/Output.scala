/**
 * User: mick
 * Date: 13.11.12
 * Time: 18:39
 */
class Output {
  private var stateParams = Map.empty[String,Any]     // holds "Set()" commands
  private var commands = ""                           // holds all other commands
  private var debugOutput = ""                        // holds all "Log()" output

  private def append(s: String) = {
    commands += (if(commands.isEmpty) s else "|" + s)
    this
  }

  override def toString = {
    var result = commands
    if(!stateParams.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += stateParams.map(e => e._1 + "=" + e._2).mkString("Set(",",",")")
    }
    if(!debugOutput.isEmpty) {
      if(!result.isEmpty) result += "|"
      result += "Log(text=" + debugOutput + ")"
    }
    result
  }

  def log(text: String) = {
    debugOutput += text + "\n"
    this
  }

  def move(direction: XY) = append("Move(direction=" + direction + ")")
  def say(text: String) = append("Say(text=" + text + ")")
  def status(text: String) = append("Status(text=" + text + ")")
  def explode(blastRadius: Int) = append("Explode(size=" + blastRadius + ")")

  def spawn(offset: XY, params: (String,Any)*) =
    append("Spawn(direction=" + offset +
      (if(params.isEmpty) "" else "," + params.map(e => e._1 + "=" + e._2).mkString(",")) +
      ")")

  def set(params: (String,Any)*) = {
    stateParams ++= params
    this
  }

  def set(keyPrefix: String, xy: XY) = {
    stateParams ++= List(keyPrefix+"x" -> xy.x, keyPrefix+"y" -> xy.y)
    this
  }
}
