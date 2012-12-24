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
  val directions = (for (x <- -1 to 1; y <- -1 to 1 if !(x == 0 && y == 0)) yield XY(x, y)).toArray
  val topRight = List(
    XY(-1,-1), XY(0,-1), XY(1,-1),
                         XY(1, 0)
  )

  val collisionScanMask = List(
    XY(-2,-2), XY(-1,-2), XY(0,-2), XY(1,-2), XY(2,-2),
    XY(-2,-1), XY(-1,-1), XY(0,-1), XY(1,-1), XY(2,-1),
    XY(-2, 0), XY(-1, 0),           XY(1, 0), XY(2, 0),
                          XY(0, 1), XY(1, 1), XY(2, 1)
  )
}
