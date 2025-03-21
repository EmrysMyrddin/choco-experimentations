import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.Model
import scala.compiletime.ops.boolean
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression
import org.chocosolver.solver.variables.view.set.SetIntsView

class PosVar(val name: String)(using model: Model, board: Board):
  val x = model.intVar(f"${name}_pos_x", 0, board.cols - 1)
  val y = model.intVar(f"${name}_pos_y", 0, board.rows - 1)

  override def toString(): String = f"($x, $y)"

extension (v: IntVar)
  def +(x: Int)          = v.add(x)
  def +(x: ArExpression) = v.add(x)

extension (a: ArExpression)
  def ===(x: IntVar) = a.eq(x)
  def *(x: Int)      = a.mul(x)

given Conversion[ArExpression, IntVar] with
  def apply(a: ArExpression) = a.intVar

given (using board: Board): Conversion[PosVar, IntVar] with
  def apply(pos: PosVar) = pos.x + pos.y * board.cols
