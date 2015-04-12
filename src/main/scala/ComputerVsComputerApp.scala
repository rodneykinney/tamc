import scala.scalajs.js
import scala.util.Random
import org.scalajs.jquery._

import TicTacToe._

object ComputerVsComputerApp extends js.JSApp {

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

  def playGame(): (Boolean, Boolean) = {
    jQuery("#playGame").prop("disabled", true)
    jQuery("#playMatch").prop("disabled", true)
    status("")
    game = new Game
    var result = game.result

    val (leftPlayer, player1Move, player2Move) =
      if (rand.nextDouble < 0.5) {
        (1,
          () => js.Dynamic.global.chooseMoveLeft(new Board(game.gameState.toArray, 1)
            .asInstanceOf[js.Any]).asInstanceOf[Int] - 1,
          () => js.Dynamic.global.chooseMoveRight(new Board(game.gameState.toArray, 2)
            .asInstanceOf[js.Any]).asInstanceOf[Int] - 1
          )
      }
      else {
        (2,
          () => js.Dynamic.global.chooseMoveRight(new Board(game.gameState.toArray, 1)
            .asInstanceOf[js.Any]).asInstanceOf[Int] - 1,
          () => js.Dynamic.global.chooseMoveLeft(new Board(game.gameState.toArray, 2)
            .asInstanceOf[js.Any]).asInstanceOf[Int] - 1
          )
      }
    val rightPlayer = 3 - leftPlayer
    var currentPlayer = 1
    val nextMove = Array(player1Move, player2Move)
    while (result == IN_PROGRESS) {
      try {
        val move = nextMove(currentPlayer-1)()
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
    (leftWin, rightWin)
  }
  
  def playMatch(): Unit = {
    val count = 1000
    var leftWins = 0
    var rightWins = 0
    for (i <- Range(0, count)) {
      val (l,r) = playGame()
      if (l) leftWins += 1
      if (r) rightWins += 1
    } 
    status(s"Left won $leftWins, Right won $rightWins, Draw ${count - leftWins - rightWins}")
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
