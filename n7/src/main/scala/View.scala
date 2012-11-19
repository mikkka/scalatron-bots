/**
 * User: mick
 * Date: 29.08.12
 * Time: 18:10
 */
case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size/2, size/2)
  val map = cells.grouped(size).map(str => str.toCharArray.map(Element(_))).toArray

  def apply(pos: XY) = map(pos.y)(pos.x)
  def from(pos: XY) = apply(center + pos)
  def elxy(pos: XY) = ElementXY(apply(pos), pos)

  def linear(p: Element => Boolean) = map.par.view.zipWithIndex.flatMap(l =>
    l._1.zipWithIndex.filter(t => {p(t._1)}).map(e => ElementXY(e._1, XY(e._2, l._2)))).toList

  def offsetToNearestElement(pos: XY, predicate: Element => Boolean) = {
    val relativePositions = linear(predicate).map(exy => ElementXY(exy.el, exy.xy - pos))
    if(relativePositions.isEmpty) None
    else Some(relativePositions.minBy(_.xy.length))
  }

  def offsets(pos: XY, predicate: Element => Boolean) = {
    val offs = linear(predicate).map(exy => ElementXY(exy.el, exy.xy - pos))
    offs
  }

  def relative(xs: List[ElementXY]) = xs.map(elxy => ElementXY(elxy.el, XY(elxy.xy.x - center.x, elxy.xy.y - center.y)))

  def part45(vec: XY) =
    relative(
    (if(math.abs(vec.x) == math.abs(vec.y)) {
      for (
        x <- if (vec.x == -1) 0 to center.x else center.x until size;
        y <- if (vec.y == -1) 0 to center.y else center.y until size
      ) yield elxy(XY(x, y))
    } else {
      for (
        height <- 0 to size / 2;
        width <- (-height) to (+height)
      ) yield {
        if (math.abs(vec.x) == 1) elxy(XY(center.x + (vec.x * height), center.y + width))
        else elxy(XY(center.x + width, center.y + (vec.y * height)))
      }
    }).filter(_.xy != center).toList)
}

abstract sealed class Element() {
  val ch: Char
  override def toString = ch.toString
  override def equals(other: Any) = other match {
    case that: Element => that.ch == this.ch
    case _ => false
  }
}

object Element {
  def apply(ch: Char) : Element = ch match {
    case Unknown.ch => Unknown
    case Empty.ch => Empty
    case Wall.ch => Wall
    case Bot.ch => Bot
    case EnemyBot.ch => EnemyBot
    case MiniBot.ch => MiniBot
    case EnemyMiniBot.ch => EnemyMiniBot
    case Zugar.ch => Zugar
    case Toxifera.ch => Toxifera
    case Fluppet.ch => Fluppet
    case Snorg.ch => Snorg
  }
}
object Unknown extends Element {
  override val ch = '?'
}
object Empty extends Element {
  override val ch = '_'
}
object Wall extends Element {
  override val ch = 'W'
}
object Bot extends Element {
  override val ch = 'M'
}
object MiniBot extends Element {
  override val ch = 'S'
}
object EnemyBot extends Element {
  override val ch = 'm'
}
object EnemyMiniBot extends Element {
  override val ch = 's'
}
object Zugar extends Element {
  override val ch = 'P'
}
object Toxifera extends Element {
  override val ch = 'p'
}
object Fluppet extends Element {
  override val ch = 'B'
}
object Snorg extends Element {
  override val ch = 'b'
}

case class ElementXY(el: Element, xy: XY)

