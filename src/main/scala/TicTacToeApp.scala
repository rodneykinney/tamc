import scala.scalajs.js.JSApp

import org.scalajs.jquery.{JQueryEventObject, jQuery}

object TicTacToeApp extends JSApp {
  def main(): Unit = {
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    renderGame(gameState)

    jQuery("#move").keydown(addMove _)
  }

  var player = 1
  var gameState = Seq.fill(9)(0).toArray

  def addMove(data: JQueryEventObject) = {
    val move: Int = data.which.toChar - '1'
    move match {
      case cell if cell >= 0 && cell < 9 && gameState(cell) == 0 =>
        gameState(cell) = player
        player = 3 - player
        renderGame(gameState)
      case _ => ()  
    }
    jQuery("#move").value("")
  }

  val cell = "-XO"

  def renderGame(state: Seq[Int]) = {
    for ((player, i) <- state.zipWithIndex) {
      jQuery(s"#cell$i").text(cell(player).toString)
    }
  }
}
