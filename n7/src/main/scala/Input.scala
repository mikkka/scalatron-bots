/**
 * User: mick
 * Date: 13.11.12
 * Time: 18:36
 */
case class Input(params: Map[String, String]) {
  val view = View(params("view"))
  val energy = params("energy").toInt
  val time = params("time").toInt
  val generation = params("generation").toInt
  val mood = inputOrElse("mood", "")
  val offsetToMaster = inputAsXYOrElse("master", XY.Zero)
  val collision = inputAsXYOrElse("collision", XY.Zero)

  val history = params.getOrElse("_history", "0:0;0:0").
    split(";").toList.
    map(s => if (!s.isEmpty) XY(s) else null).
    filter(_ != null).sliding(2).
    zipWithIndex.filter(_._2 % 2 == 0).map(_._1).collect{case List(move, coord) => (move, coord)}.toList

  val moves = history.map(_._1)
  val coords = history.map(_._2)

  def inputOrElse(key: String, fallback: String) = params.getOrElse(key, fallback)
  def inputAsIntOrElse(key: String, fallback: Int) = params.get(key).map(_.toInt).getOrElse(fallback)
  def inputAsXYOrElse(key: String, fallback: XY) = params.get(key).map(s => XY(s)).getOrElse(fallback)
}
