import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.Model
import scala.compiletime.ops.boolean
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression
import org.chocosolver.solver.variables.view.set.SetIntsView


trait PiecePart
case object ⬛ extends PiecePart
case object ⬜ extends PiecePart


class Piece(val name: String, val color: String, val parts: Seq[(Int, Int)]):
  def rotate(direction: Int) = parts.map(rotateVec(direction, _))


object Piece:
  def apply(name: String, color: String)[T <: Tuple](grid: T*) =
    val parts: Seq[(Int, Int)] =
      val cols = grid.map(_.toList.length).max
      for
        y <- 0 until grid.length
        x <- 0 until cols if grid(y)(x) == ⬛
      yield (x, y)
    new Piece(name, color, parts)


def showParts(color: String, parts: Seq[(Int, Int)]) =
  var res = StringBuilder();
  val max = parts.unzip.toList.flatten.map(Math.abs).max

  for
    y <- 0 to max
    x <- 0 to max
  yield
    res.append(
      if parts contains (x, y) then f"\u001b[38;5;${color}m⬛"
      else "\u001b[0m⬜"
    )
    if (x == max) res.append('\n')

  println(res)


def rotateVec(direction: Int, vec: (Int, Int)): (Int, Int) =
  (direction, vec) match
    case (0, (x, y))          => (x, y)
    case (1, (x, y))          => (-y, x)
    case (2, (x, y))          => (-x, -y)
    case (3, (x, y))          => (y, -x)
    case (d, (x, y)) if d < 8 => rotateVec(d - 4, (x, -y))
    case (d, _) => throw Error(f"Can't rotate with direction $direction")


def translateToOrigin(parts: (Int, Int)*) =
  val minX :: minY :: _ = parts.unzip.toList.map(_.min): @unchecked;
  val (trX, trY)        = (0 - minX, 0 - minY);

  parts map { part =>
    part match
      case (x, y) => (x + trX, y + trY)
  }

