import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.Model
import scala.compiletime.ops.boolean
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression
import org.chocosolver.solver.variables.view.set.SetIntsView

class Card(val rows: Int, val pieces: Piece*):
  def board()(using Model) =
    val board = Board(5, rows)
    board.addCard(this)
    board

val tetris = Piece("tetris", "221")(
  (⬛, ⬛, ⬜),
  (⬜, ⬛, ⬛)
)

val square = Piece("square", "170")(
  (⬛, ⬛),
  (⬛, ⬛)
)

val line3 = Piece("line3", "208")(
  (⬛, ⬛, ⬛)
)

val angle2 = Piece("angle2", "130")(
  (⬛, ⬜),
  (⬛, ⬛)
)

val angle3 = Piece("angle3", "21")(
  (⬛, ⬜, ⬜),
  (⬛, ⬜, ⬜),
  (⬛, ⬛, ⬛)
)

val point = Piece("point", "196")(
  (⬛, ⬜)
)

val line2 = Piece("line2", "216")(
  (⬛, ⬛)
)

val snake = Piece("snake", "176")(
  (⬜, ⬜, ⬛, ⬛),
  (⬛, ⬛, ⬛, ⬜),
)

val longL = Piece("longL", "208")(
  (⬛, ⬛, ⬛, ⬛),
  (⬜, ⬜, ⬜, ⬛),
)

val shortL = Piece("shortL", "114")(
  (⬛, ⬛, ⬛),
  (⬛, ⬜, ⬜),
)

val escargot = Piece("escargot", "206")(
  (⬛, ⬜),
  (⬛, ⬛),
  (⬛, ⬛)
)

val u = Piece("u", "226")(
  (⬛, ⬜, ⬛),
  (⬛, ⬛, ⬛)
)

val s = Piece("s", "39")(
  (⬛, ⬛, ⬜),
  (⬜, ⬛, ⬜),
  (⬜, ⬛, ⬛)
)
