import TicTacToe._

class Game {
  val board = new BoardView
  var player = 1
  var gameState = Seq.fill(9)(0).toArray
  board.update(gameState)
  var result = IN_PROGRESS

  def move(cell: Int): Unit = cell match {
    case _ if cell >= 0 && cell < 9 && gameState(cell) == 0 =>
      gameState(cell) = player
      player = 3 - player
      board.update(gameState)
      result = outcome(gameState)
    case _ => ()
  }
}
