import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.Model
import scala.compiletime.ops.boolean
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression
import org.chocosolver.solver.variables.view.set.SetIntsView

class Board(val cols: Int, val rows: Int):
  given Board = this
  var vars    = Seq[IntVar]()
  var colors  = Map[IntVar, String]()

  def post()(using model: Model) =
    model.allDifferent(vars*).post();

  def addCard(card: Card)(using Model) = addPiece(card.pieces*)

  def addPiece(pieces: Piece*)(using Model): Unit = pieces.foreach(addPiece)
  def addPiece(piece: Piece)(using model: Model) =
    val pos = PosVar(piece.name)
    val partVars =
      piece.parts.zipWithIndex.map((_, i) => PosVar(f"${piece.name}_p${i}"))

    partVars
      .map(_.convert)
      .foreach({ part =>
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
          model.arithm(partVars(i).x, "-", pos.x, "=", x),
          model.arithm(partVars(i).y, "-", pos.y, "=", y),
        )
      yield rule
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
