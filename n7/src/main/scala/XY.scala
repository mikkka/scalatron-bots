/**
 * User: mick
 * Date: 29.08.12
 * Time: 18:10
 */
case class XY(x: Int, y: Int) {
  override def toString = x + ":" + y
  def +(pos: XY) = XY(x+pos.x, y+pos.y)
  def -(pos: XY) = XY(x-pos.x, y-pos.y)
  def distanceTo(pos: XY) : Double = (this-pos).length
  //TODO: переделать на расстояния в шагах
  def length : Double = math.max(math.abs(x), math.abs(y))
  def signum = XY(x.signum, y.signum)
  def scalar(o: XY): Double = this.x * o.x + this.y * o.y
  def toEntityName = x + "_" + y
}

object XY {
  def apply(s: String) : XY = {
    val a = s.split(':')
    XY(a(0).toInt, a(1).toInt)
  }

  val Zero = XY(0, 0)
}