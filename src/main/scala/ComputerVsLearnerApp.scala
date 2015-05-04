import TicTacToe._
import org.scalajs.jquery._

import scala.scalajs.js

object ComputerVsLearnerApp extends ComputerVsComputer with js.JSApp {

  var robot: Robot = Map().withDefaultValue(1.0)
  var trainer: RobotTrainer = simpleTrainer

  override def chooseMoveRight = {
    val state = game.gameState
    selectMove(state, validMoves(state), robot)
  }

  override def playGame() = {
    val (leftPlayerId, rightPlayerId) = super.playGame()
    val (leftWin, rightWin, leftPlayerHistory, rightPlayerHistory) =
      (game.playerWon(leftPlayerId),
        game.playerWon(rightPlayerId),
        game.playerMoveHistory(leftPlayerId),
        game.playerMoveHistory(rightPlayerId))
    robot = trainer(RobotTrainingData(robot, rightPlayerHistory, rightWin, leftWin))
    (leftPlayerId, rightPlayerId)
  }

  override def setupUI: Unit = {
    super.setupUI
    jQuery("#inputLeft").prop("disabled", true)
  }
}
