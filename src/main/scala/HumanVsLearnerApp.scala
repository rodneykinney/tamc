import TicTacToe._
import org.scalajs.jquery._

import scala.scalajs.js

/**
 * Created by rodneykinney on 5/3/15.
 */
object HumanVsLearnerApp extends HumanVsComputer with js.JSApp {
  var robot: Robot = Map().withDefaultValue(1.0)

  var trainer: RobotTrainer = simpleTrainer

  val wView = new WeightView

  override def selectComputerPlayerMove(state: Seq[Int]) = {
    val choices = validMoves(state)
    val selected = selectMove(state, choices, robot)
    wView.update(state, robot)
    selected
  }

  override def gameOver(result: Int) = {
    super.gameOver(result)
    val trainData =
      RobotTrainingData(robot,
        game.playerMoveHistory(computerPlayer),
        game.playerWon(computerPlayer),
        game.playerWon(3 - computerPlayer)
      )
    robot = trainer(trainData)
  }

  override def startGame(event: JQueryEventObject) = {
    super.startGame(event)
    wView.update(game.gameState, robot)
  }

}

class WeightView {
  val cellValue = "-XO"

  def update(state: Seq[Int], robot: Robot) = {
    for ((player, i) <- state.zipWithIndex) {
      player match {
        case 0 => jQuery(s"#wcell$i").text("%.2f".format(robot((state, i))))
        case _ => jQuery(s"#wcell$i").text(cellValue(player).toString)
      }
    }
  }

}