import scala.scalajs.js.JSApp

import org.scalajs.jquery.{JQueryEventObject, jQuery}
import TicTacToe._

object HumanVsHumanApp extends JSApp {
  def main(): Unit = {
    jQuery(setupUI _)
  }

  def setupUI(): Unit = {
    jQuery("#move").keydown(addMove _)
    jQuery("#startGame").click(startGame _)
  }

  var game: Game = _

  def startGame(event: JQueryEventObject) = {
    game = new Game()
    jQuery("#move").prop("disabled",false)
  }

  def addMove(event: JQueryEventObject) = {
    val move: Int = event.which.toChar - '1'
    game.move(move)
    jQuery("#move").value("")
    game.result match {
      case FIRST_PLAYER_WIN =>
        jQuery("#move").prop("disabled",true)
      case SECOND_PLAYER_WIN =>
        jQuery("#move").prop("disabled",true)
      case _ => ()
    }
  }
}
