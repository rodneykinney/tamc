import TicTacToe._
import org.scalajs.jquery._

import scala.scalajs.js

object LearnerVsLearnerApp extends ComputerVsComputer with js.JSApp {

  var leftRobot: Robot = Map().withDefaultValue(1.0)
  var rightRobot: Robot = Map().withDefaultValue(1.0)
  var rightWeightUpdater: WeightUpdater =
    (won: Boolean, lost: Boolean, oldWeight: Double, moveNumber, gameLength) =>
      js.Dynamic.global.chooseMoveRight(won, lost, oldWeight, moveNumber, gameLength).asInstanceOf[Double]
  var leftWeightUpdater: WeightUpdater =
    (won: Boolean, lost: Boolean, oldWeight: Double, moveNumber, gameLength) =>
      js.Dynamic.global.chooseMoveLeft(won, lost, oldWeight, moveNumber, gameLength).asInstanceOf[Double]

  override def chooseMoveRight = {
    val state = game.gameState
    selectMove(state, validMoves(state), rightRobot)
  }
  override def chooseMoveLeft = {
    val state = game.gameState
    selectMove(state, validMoves(state), leftRobot)
  }

  override def playGame() = {
    val (leftPlayerId, rightPlayerId) = super.playGame()
    val (leftWin, rightWin, leftPlayerHistory, rightPlayerHistory) =
      (game.playerWon(leftPlayerId),
        game.playerWon(rightPlayerId),
        game.playerMoveHistory(leftPlayerId),
        game.playerMoveHistory(rightPlayerId))
    val rightTrainData = RobotTrainingData(rightRobot, rightPlayerHistory, rightWin, leftWin)
    rightRobot = trainRobot(rightWeightUpdater)(rightTrainData)
    val leftTrainData = RobotTrainingData(leftRobot, leftPlayerHistory, leftWin, rightWin)
    leftRobot = trainRobot(leftWeightUpdater)(leftTrainData)
    (leftPlayerId, rightPlayerId)
  }

  override def setupUI: Unit = {
    super.setupUI
  }
}
