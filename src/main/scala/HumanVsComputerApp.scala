import TicTacToe._
import org.scalajs.jquery._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Random

object HumanVsComputerApp extends js.JSApp {

  val rand = new Random()

  def main(): Unit = {
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    jQuery("#startGame").click(startGame _)
    jQuery("#input").keyup(validateInput _)
    jQuery("#move").keydown(addMove _)
    validateInput(null)
  }

  def validateInput(event: JQueryEventObject): Unit = {
    try {
      val code = jQuery("#input").value
      js.eval( s"""var chooseMove = ${code.toString};""")
      jQuery("#startGame").prop("disabled", false)
    }
    catch {
      case ex: Exception =>
        jQuery("#startGame").prop("disabled", true)

    }
  }

  def computerPlayerMove(state: Seq[Int]) = {
    val cells = state.zipWithIndex
    val move = js.Dynamic.global.chooseMove(
      new Board(state.toArray, computerPlayer).asInstanceOf[js.Any])
      .asInstanceOf[Int]
    move - 1
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
      case IN_PROGRESS => false
      case _ =>
        jQuery("#move").prop("disabled", true)
        true
    }
  }

  def startGame(event: JQueryEventObject) = {
    game = new Game()
    jQuery("#move").text("")
    jQuery("#move").prop("disabled", false)
    if (rand.nextDouble() < 0.5) {
      computerPlayer = 1
      game.move(computerPlayerMove(game.gameState))
    }
  }
}

@JSExport
class Board(state: Array[Int], computerPlayer: Int) {
  val cells = state.zipWithIndex.map{case (player,i) => (player, i+1)}

  import js.JSConverters._

  @JSExport
  def unoccupied = empty.toJSArray

  val empty = cells.filter(_._1 == 0).map(_._2)
  val mine = cells.filter(_._1 == 3 - computerPlayer).map(_._2)
  val occupiedByOpponent = cells.filter(_._1 == computerPlayer).map(_._2)

  @JSExport
  def randomMove() = {
    empty(HumanVsComputerApp.rand.nextInt(empty.size))
  }


}
