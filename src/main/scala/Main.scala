import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.Model
import scala.compiletime.ops.boolean
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression

@main def hello(): Unit =
  given model: Model("gagne ton papa");
  given board: Board(5, 3);

  board.addPiece(
    Piece(
      "tetris",
      "221",
      (⬛, ⬛, ⬜),
      (⬜, ⬛, ⬛)
    )
  );
  board.addPiece(
    Piece(
      "square",
      "170",
      (⬛, ⬛),
      (⬛, ⬛)
    )
  );
  board.addPiece(
    Piece(
      "line3",
      "208",
      (⬛, ⬛, ⬛)
    )
  )
  board.addPiece(
    Piece(
      "angle2",
      "130",
      (⬛, ⬜),
      (⬛, ⬛)
    )
  )
  board.addPiece(
    Piece(
      "point",
      "196",
      (⬛, ⬜)
    )
  )

  board.post();

  while model.getSolver().findSolution() != null
  do board.print();

class Board(val cols: Int, val rows: Int):
  given Board = this
  var vars    = Seq[IntVar]()
  var colors  = Map[IntVar, String]()

  def post()(using model: Model) =
    model.allDifferent(vars*).post();

  def addPiece[T <: Tuple](
      piece: Piece[T]
  )(using model: Model) =
    val partVars =
      piece.parts.zipWithIndex.map((_, i) => PosVar(f"${piece.name}_p${i}"))

    partVars
      .map(_.convert)
      .foreach({ (part: IntVar) =>
        colors = colors + ((part, piece.color))
        vars = vars :+ part
      })

    val rotations =
      for direction <- 0 until 8
      yield translateToOrigin(piece.rotate(direction)*).sorted

    rotations.distinct.foreach(showParts(piece.color, _))

    val rotationsConstraints = rotations.distinct.map(rotation =>
      val constraints = for
        ((x, y), i) <- rotation.zipWithIndex
        rule <- Seq(
          piece.pos.x + x === partVars(i).x,
          piece.pos.y + y === partVars(i).y
        )
      yield rule.decompose
      model.and(constraints*)
    )

    model.or(rotationsConstraints*).post()

  def print() =
    Predef.print("\u001b[0m==== Solution ====");
    for
      y <- 0 until rows
      x <- 0 until cols
      pos   = x + y * cols
      piece = vars find (_.getValue == pos)
    yield
      if x == 0 then Predef.print('\n');
      piece match
        case None => Predef.print("\u001b[0m⬜")
        case Some(p) =>
          Predef.print(f"\u001b[38;5;${colors.getOrElse(p, "251")}m⬛")
    Predef.print("\u001b[0m\n")

  def debug() = println(vars)

class Piece[T <: Tuple](val name: String, val color: String, grid: T*)(using
    Model,
    Board
):
  val pos = PosVar(name)

  val parts: Seq[(Int, Int)] =
    val cols = grid.map(_.toList.length).max
    for
      y <- 0 until grid.length
      x <- 0 until cols if grid(y)(x) == ⬛
    yield (x, y)

  def rotate(direction: Int) = parts.map(rotateVec(direction, _))

  override def toString(): String =
    f"(${pos.x.getValue()}, ${pos.y.getValue()})"

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

class Vec(val x: Int, val y: Int):
  def rotate() = Vec(x, y)

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

trait PiecePart
case object ⬛ extends PiecePart
case object ⬜ extends PiecePart

def showParts(color: String, parts: Seq[(Int, Int)]) =
  var res = StringBuilder();
  val max = parts.unzip.toList.flatten.map(Math.abs).max

  for
    y <- -max to max
    x <- -max to max
  yield
    res.append(
      if parts contains (x, y) then f"\u001b[38;5;${color}m⬛"
      else "\u001b[0m⬜"
    )
    if (x == max) res.append('\n')

  println(res)
