/**
 * User: mick
 * Date: 13.11.12
 * Time: 18:36
 */
class Input(params: Map[String, String]) {
  val view = View(params("view"))
  val energy = params("energy").toInt
  val time = params("time").toInt
  val generation = params("generation").toInt
  def offsetToMaster = inputAsXYOrElse("master", XY.Zero)

  def inputOrElse(key: String, fallback: String) = params.getOrElse(key, fallback)
  def inputAsIntOrElse(key: String, fallback: Int) = params.get(key).map(_.toInt).getOrElse(fallback)
  def inputAsXYOrElse(key: String, fallback: XY) = params.get(key).map(s => XY(s)).getOrElse(fallback)
}
