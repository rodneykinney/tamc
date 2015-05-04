import TicTacToe._
import org.scalajs.jquery._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExport
import scala.util.Random

object HumanVsComputerApp extends HumanVsComputer with js.JSApp

class HumanVsComputer {

  val rand = new Random()

  def main(): Unit = {
    jQuery(setupUI _)
  }

  val computerInput: ComputerPlayerInput =
    new ComputerPlayerInput("input", "chooseMove", validateInput)

  def setupUI(): Unit = {
    jQuery("#startGame").click(startGame _)
    computerInput.setupUI()
    jQuery("#move").keydown(humanPlayerMove _)
  }

  def validateInput(isValid: Boolean): Unit = {
    jQuery("#startGame").prop("disabled", !isValid)
  }

  def selectComputerPlayerMove(state: Seq[Int]) =
    js.Dynamic.global.chooseMove(
      new Board(state.toArray, computerPlayer).asInstanceOf[js.Any]).asInstanceOf[Int] - 1

  def computerPlayerMove(state: Seq[Int]) = {
    try {
      val move = selectComputerPlayerMove(state)
      if (!game.validMove(move)) {
        gameOver(3 - computerPlayer)
      }
      else {
        game.move(move)
        checkGameEnd
      }
    }
    catch {
      case ex: Exception =>
        ex.printStackTrace()
        gameOver(3 - computerPlayer)
    }
    jQuery("#move").value("")
  }

  var game: Game = _
  var computerPlayer = 2

  def humanPlayerMove(event: JQueryEventObject) = {
    val move: Int = event.which.toChar - '1'
    if (game.validMove(move)) {
      game.move(move)
      if (!checkGameEnd) {
        computerPlayerMove(game.gameState)
      }
    }
    else {
      status("Invalid move")
    }
    jQuery("#move").value("")
  }

  def gameOver(result: Int): Unit = {
    val msg = result match {
      case _ if result == computerPlayer => "Computer player wins!!"
      case _ if result == 3 - computerPlayer => "Human player wins!!"
      case _ => "Draw"
    }
    status(msg)
    jQuery("#move").prop("disabled", true)
  }

  def checkGameEnd = {
    game.result match {
      case IN_PROGRESS => false
      case _ =>
        gameOver(game.result)
        true
    }
  }

  def startGame(event: JQueryEventObject): Unit = {
    game = new Game()
    jQuery("#move").text("")
    jQuery("#move").prop("disabled", false)
    status("Playing game ...")
    if (rand.nextDouble() < 0.5) {
      computerPlayer = 1
      computerPlayerMove(game.gameState)
    }
    else {
      computerPlayer = 2
    }
  }
}

@JSExport
class Board(state: Array[Int], computerPlayer: Int) {
  val cells = state.zipWithIndex.map { case (player, i) => (player, i + 1)}

  import HumanVsComputerApp.rand

  @JSExport
  def unoccupied = empty.toJSArray

  @JSExport
  val empty = cells.filter(_._1 == 0).map(_._2).toJSArray
  @JSExport
  val occupiedByOpponent = cells.filter(_._1 == 3 - computerPlayer).map(_._2).toJSArray
  @JSExport
  val occupiedByMe = cells.filter(_._1 == computerPlayer).map(_._2).toJSArray

  @JSExport
  def chooseRandom(items: js.Array[js.Any]) = {
    items(rand.nextInt(items.size))
  }

  @JSExport
  def isEmpty(cell: Int): Boolean = {
    cells(cell - 1)._1 == 0
  }

  @JSExport
  def isOccupiedByMe(cell: Int): Boolean = {
    cells(cell - 1)._1 == computerPlayer
  }

  @JSExport
  def isOccupiedByOpponent(cell: Int): Boolean = {
    cells(cell - 1)._1 == 3 - computerPlayer
  }

  @JSExport
  def lines: js.Array[Row] = {
    val horizontal = for (i <- List(0, 3, 6)) yield
      new Row(Array(cells(i), cells(i + 1), cells(i + 2)), computerPlayer)
    val vertical = for (i <- List(0, 1, 2)) yield
      new Row(Array(cells(i), cells(i + 3), cells(i + 6)), computerPlayer)
    val diagonal = Array(
      new Row(Array(cells(0), cells(4), cells(8)), computerPlayer),
      new Row(Array(cells(2), cells(4), cells(6)), computerPlayer))
    (horizontal ++ vertical ++ diagonal).toJSArray
  }
}

@JSExport
class Row(cells: Array[(Int, Int)], computerPlayer: Int) {
  @JSExport
  val empty = cells.filter(_._1 == 0).map(_._2).toJSArray
  @JSExport
  val occupiedByOpponent = cells.filter(_._1 == 3 - computerPlayer).map(_._2).toJSArray
  @JSExport
  val occupiedByMe = cells.filter(_._1 == computerPlayer).map(_._2).toJSArray

  override def toString = s"""Row[${cells.map(x => s"(${x._1},${x._2})").mkString("")}]"""
}