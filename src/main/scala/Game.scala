import TicTacToe._

import scala.collection.mutable.ListBuffer

class Game {
  val board = new BoardView
  var player = 1
  def gameState = _gameState.toList
  private var _gameState = Seq.fill(9)(0).toArray
  board.update(gameState)
  var result = IN_PROGRESS
  private val playerHistories =
    Array(ListBuffer.empty[(Seq[Int], Int)], ListBuffer.empty[(Seq[Int], Int)])
  def playerMoveHistory(player: Int) = playerHistories(player-1).toList
  def playerWon(player: Int) = result == player

  def validMove(cell: Int): Boolean = cell >= 0 && cell < 9 && gameState(cell) == 0

  def move(cell: Int): Boolean = cell match {
    case _ if validMove(cell) =>
      playerHistories(player-1) += ((gameState, cell))
      _gameState(cell) = player
      player = 3 - player
      board.update(gameState)
      result = outcome(gameState)
      true
    case _ => false
  }

}
