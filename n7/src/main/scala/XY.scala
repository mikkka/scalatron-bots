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
  def length : Double = math.sqrt(x*x + y*y)
  def signum = XY(x.signum, y.signum)
  def toEntityName = x + "_" + y
}
