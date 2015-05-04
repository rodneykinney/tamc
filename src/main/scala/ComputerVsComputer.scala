import TicTacToe._
import org.scalajs.jquery._

import scala.collection.mutable.ListBuffer
import scala.scalajs.js
import scala.util.Random

object ComputerVsComputerApp extends ComputerVsComputer with js.JSApp

class ComputerVsComputer {

  val rand = new Random()

  var game: Game = _

  def main(): Unit = {
    jQuery(setupUI _)
  }

  val leftPlayerInput =
    new ComputerPlayerInput("inputLeft", "chooseMoveLeft", validateInputLeft)
  val rightPlayerInput =
    new ComputerPlayerInput("inputRight", "chooseMoveRight", validateInputRight)

  def setupUI: Unit = {
    leftPlayerInput.setupUI()
    rightPlayerInput.setupUI()
    jQuery("#playGame").click(playGame _)
    jQuery("#playMatch").click(playMatch _)
  }

  def chooseMoveLeft = {
    js.Dynamic.global.chooseMoveLeft(new Board(game.gameState.toArray, 1)
      .asInstanceOf[js.Any]).asInstanceOf[Int] - 1
  }

  def chooseMoveRight = {
    js.Dynamic.global.chooseMoveRight(new Board(game.gameState.toArray, 2)
      .asInstanceOf[js.Any]).asInstanceOf[Int] - 1
  }

  type MoveChoiceHistory = List[(Seq[Int], Int)]

  def playGame(): (Int, Int) = {
    jQuery("#playGame").prop("disabled", true)
    jQuery("#playMatch").prop("disabled", true)
    status("")
    game = new Game
    var result = game.result

    val (leftPlayer, player1Move, player2Move) =
      if (rand.nextDouble < 0.5) {
        (1,
          () => chooseMoveLeft,
          () => chooseMoveRight
          )
      }
      else {
        (2,
          () => chooseMoveRight,
          () => chooseMoveLeft
          )
      }
    val rightPlayer = 3 - leftPlayer
    var currentPlayer = 1
    val nextMove = Array(player1Move, player2Move)
    while (result == IN_PROGRESS) {
      try {
        val move = nextMove(currentPlayer - 1)()
        game.move(move)
        result = game.result
      }
      catch {
        case ex: Exception =>
          ex.printStackTrace()
          result = 3 - currentPlayer
      }
      currentPlayer = 3 - currentPlayer
    }
    val leftWin = result == leftPlayer
    val rightWin = result == rightPlayer
    val symbol = "-XO"
    gameOver(leftWin, rightWin, symbol(leftPlayer), symbol(rightPlayer))
    jQuery("#playGame").prop("disabled", false)
    jQuery("#playMatch").prop("disabled", false)
    (leftPlayer, rightPlayer)
  }

  def playMatch(): (List[Int], List[Int]) = {
    val count = 1000
    val leftWinHistory = new ListBuffer[Int]
    val rightWinHistory = new ListBuffer[Int]
    for (i <- Range(0, count)) {
      val (l, r) = playGame()
      leftWinHistory += (if (game.playerWon(l)) 1 else 0)
      rightWinHistory += (if (game.playerWon(r)) 1 else 0)
    }
    val leftWins = leftWinHistory.sum
    val rightWins = rightWinHistory.sum
    status(s"Left won $leftWins, Right won $rightWins, Draw ${count - leftWins - rightWins}")
    (leftWinHistory.toList, rightWinHistory.toList)
  }

  def gameOver(leftWin: Boolean, rightWin: Boolean, leftSymbol: Character, rightSymbol: Character):
  Unit = {
    val msg = (leftWin, rightWin) match {
      case (true, false) => s"Left player ($leftSymbol) wins!!"
      case (false, true) => s"Right player ($rightSymbol) wins!!"
      case (false, false) => "Draw"
      case _ => "Error"
    }
    status(msg)
  }

  var leftValid: Boolean = false
  var rightValid: Boolean = false

  def validateInputLeft(isValid: Boolean): Unit = {
    leftValid = isValid
    jQuery("#playGame").prop("disabled", !(leftValid && rightValid))
    jQuery("#playMatch").prop("disabled", !(leftValid && rightValid))
  }

  def validateInputRight(isValid: Boolean): Unit = {
    rightValid = isValid
    jQuery("#playGame").prop("disabled", !(leftValid && rightValid))
    jQuery("#playMatch").prop("disabled", !(leftValid && rightValid))
  }
}
