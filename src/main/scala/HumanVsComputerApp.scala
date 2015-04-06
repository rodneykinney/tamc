import TicTacToe._
import org.scalajs.jquery._

import scala.scalajs.js
import scala.scalajs.js._
import scala.util.Random

object HumanVsComputerApp extends JSApp {

  val rand = new Random()

  def main(): Unit = {
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    jQuery("#startGame").click(startGame _)
    jQuery("#input").keyup(validateInput _)
    jQuery("#move").keydown(addMove _)

  }

  def validateInput(event: JQueryEventObject): Unit = {
    try {
      val code = jQuery("#input").value
      eval( s"""var chooseMove = ${code.toString};""")
      jQuery("#startGame").prop("disabled", false)
    }
    catch {
      case ex: Exception =>
        jQuery("#startGame").prop("disabled", true)

    }
  }

  def computerPlayerMove(state: Seq[Int]) = {
    val cells = state.zipWithIndex
    val unoccupied = cells.filter(_._1 == 0).map(_._2)
    val occupiedByMe = cells.filter(_._1 == 3 - computerPlayer).map(_._2)
    val occupiedByOpponent = cells.filter(_._1 == computerPlayer).map(_._2)
    val move = js.Dynamic.global.chooseMove(unoccupied, occupiedByMe, occupiedByOpponent)
      .asInstanceOf[Int]
    move
  }

  var game: Game = _
  var computerPlayer = 2

  def addMove(event: JQueryEventObject) = {
    val move: Int = event.which.toChar - '1'
    game.move(move)
    jQuery("#move").value("")
    if (!checkGameEnd) {
      game.move(computerPlayerMove(game.gameState))
      checkGameEnd
    }
  }

  def checkGameEnd = {
    game.result match {
      case FIRST_PLAYER_WIN =>
        jQuery("#move").prop("disabled", true)
        true
      case SECOND_PLAYER_WIN =>
        jQuery("#move").prop("disabled", true)
        true
      case _ =>
        false
    }
  }

  def startGame(event: JQueryEventObject) = {
    game = new Game()
    //    if (rand.nextDouble() < 0.5) {
    //      game.move(Dynamic.global.chooseMove(game.gameState))
    //    }
  }

  js.Dynamic.global.e

}
