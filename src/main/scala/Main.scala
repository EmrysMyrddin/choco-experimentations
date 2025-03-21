import org.chocosolver.solver.variables.IntVar
import org.chocosolver.solver.Model
import scala.compiletime.ops.boolean
import org.chocosolver.solver.expression.discrete.arithmetic.ArExpression
import org.chocosolver.solver.variables.view.set.SetIntsView

@main def hello(): Unit =
  given model: Model("gagne ton papa");
  given board: Board = card49.board();

  board.post();

  val solver   = model.getSolver;
  val solution = solver.findSolution();

  println(
    f"pieces: ${card49.pieces.length}, parts: ${card49.pieces.map(p => p.parts.length).sum}"
  )

  if solution == null
  then println("No solution found");
  else board.print();

  solver.printStatistics;

  // while model.getSolver().findSolution() != null
  // do
  //   board.print();
  //   model.getSolver().printStatistics()

val card4_2     = Card(3, square, tetris, line3, angle2, point)
val card49      = Card(4, line2, tetris, shortL, longL, snake)
val cardSolo1_1 = Card(7, point, tetris, angle3, escargot, snake, u, longL, s);
