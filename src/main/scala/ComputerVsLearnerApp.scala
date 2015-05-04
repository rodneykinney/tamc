import TicTacToe._
import org.scalajs.jquery._

import scala.scalajs.js

object ComputerVsLearnerApp extends ComputerVsComputer with js.JSApp {

  var robot: Robot = Map().withDefaultValue(1.0)
  var weightUpdater: WeightUpdater =
    (won: Boolean, lost: Boolean, oldWeight: Double, moveNumber, gameLength) =>
      js.Dynamic.global.chooseMoveRight(won, lost, oldWeight, moveNumber, gameLength).asInstanceOf[Double]

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
    val trainData = RobotTrainingData(robot, rightPlayerHistory, rightWin, leftWin)
    robot = trainRobot(weightUpdater)(trainData)
    (leftPlayerId, rightPlayerId)
  }

  override def setupUI: Unit = {
    super.setupUI
    jQuery("#inputLeft").prop("disabled", true)
  }
}
