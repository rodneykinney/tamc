import org.scalajs.jquery._

import scala.collection.mutable.ListBuffer

object TicTacToe {

  type Robot = Map[(Seq[Int], Int), Double]

  type RobotTrainer = RobotTrainingData => Robot

  case class RobotTrainingData(
                                player: Robot,
                                moveHistory: Seq[(Seq[Int], Int)],
                                won: Boolean,
                                lost: Boolean)

  def validMoves(gameState: Seq[Int]): Seq[Int] =
    gameState.zipWithIndex.filter(_._1 == 0).map(_._2)

  val rand = new scala.util.Random

  def selectMove(gameState: Seq[Int], choices: Seq[Int], weights: Robot) = {

    val choiceWeights = choices.map(c => weights.withDefaultValue(1.0)((gameState, c)))
    val totalWeights = choiceWeights.sum
    val cumulativeWeights = choiceWeights.zipWithIndex.map {
      case (choice, i) => choiceWeights.take(i + 1).sum
    }
    val toss = rand.nextDouble() * totalWeights
    val selection = choices.zip(cumulativeWeights).dropWhile(toss >= _._2).head._1
    selection
  }

  val FIRST_PLAYER_WIN = 1
  val SECOND_PLAYER_WIN = 2
  val DRAW = -1
  val IN_PROGRESS = 0

  def outcome(gameState: Seq[Int]) = {
    val rows = List(
      List(0, 3, 6),
      List(1, 4, 7),
      List(2, 5, 8),
      List(0, 1, 2),
      List(3, 4, 5),
      List(6, 7, 8),
      List(0, 4, 8),
      List(2, 4, 6))
    def occupiesWinningRows(player: Int) = rows.find(_.forall(p => gameState(p) == player)).nonEmpty
    if (occupiesWinningRows(1)) {
      FIRST_PLAYER_WIN
    }
    else if (occupiesWinningRows(2)) {
      SECOND_PLAYER_WIN
    }
    else if (gameState.forall(p => p != 0)) {
      DRAW
    }
    else {
      IN_PROGRESS
    }
  }

  def playGame(player1: Robot, player2: Robot) = {
    var state: Seq[Int] = initialState
    var gameHistory = new ListBuffer[(Seq[Int], Int)]
    var result = IN_PROGRESS
    var player = player1
    var playerId = 1
    while (result == IN_PROGRESS) {
      val move = selectMove(state, validMoves(state), player)
      gameHistory += ((state, move))
      state = (state.take(move) :+ playerId) ++ state.drop(move + 1)
      result = outcome(state)
      if (playerId == 1) {
        player = player2
        playerId = 2
      }
      else {
        player = player1
        playerId = 1
      }
    }
    (result, gameHistory)
  }

  def playTournament(trainer1: RobotTrainer, trainer2: RobotTrainer) = {
    var robot1: Robot = Map()
    var robot2: Robot = Map()
    var gamesPlayed = 0
    val player1ScoreHistory = new ListBuffer[Int]
    var player2ScoreHistory = new ListBuffer[Int]
    while (gamesPlayed < 10000) {
      val (result1, history1) = playGame(robot1, robot2)
      val (result2, history2) = playGame(robot2, robot1)
      robot1 = trainer1(
        RobotTrainingData(robot1,
          history1.zipWithIndex.filter(_._2 % 2 == 0).map(_._1),
          result1 == FIRST_PLAYER_WIN,
          result1 == SECOND_PLAYER_WIN))
      robot2 = trainer2(
        RobotTrainingData(robot2,
          history2.zipWithIndex.filter(_._2 % 2 == 0).map(_._1),
          result2 == FIRST_PLAYER_WIN,
          result2 == SECOND_PLAYER_WIN))
      robot1 = trainer1(
        RobotTrainingData(robot1,
          history2.zipWithIndex.filter(_._2 % 2 == 1).map(_._1),
          result1 == SECOND_PLAYER_WIN,
          result1 == FIRST_PLAYER_WIN))
      robot2 = trainer2(
        RobotTrainingData(robot2,
          history1.zipWithIndex.filter(_._2 % 2 == 1).map(_._1),
          result2 == SECOND_PLAYER_WIN,
          result1 == FIRST_PLAYER_WIN))

      result1 match {
        case FIRST_PLAYER_WIN =>
          player1ScoreHistory += 1
          player2ScoreHistory += 0
        case SECOND_PLAYER_WIN =>
          player1ScoreHistory += 0
          player2ScoreHistory += 1
        case _ =>
          player1ScoreHistory += 0
          player2ScoreHistory += 0
      }
      result2 match {
        case FIRST_PLAYER_WIN =>
          player1ScoreHistory += 0
          player2ScoreHistory += 1
        case SECOND_PLAYER_WIN =>
          player1ScoreHistory += 1
          player2ScoreHistory += 0
        case _ =>
          player1ScoreHistory += 0
          player2ScoreHistory += 0
      }
      gamesPlayed += 2
    }
    println("Player 1 wins: %05d - Player 2 wins: %05d - Total: %05d".format(
      player1ScoreHistory.sum,
      player2ScoreHistory.sum,
      gamesPlayed))
    (player1ScoreHistory.toList, player2ScoreHistory.toList)
  }

  val initialState = Array(0, 0, 0, 0, 0, 0, 0, 0, 0)

  def displayGame(gameState: Seq[Int]): Unit = {
    println("")
    renderGame(gameState).foreach(println)
    println("")
  }

  def renderGame(gameState: Seq[Int]): Seq[String] = {
    def renderCell(player: Int) = player match {
      case 0 => " - "
      case 1 => " X "
      case 2 => " O "
    }
    Seq(gameState.take(3).map(renderCell).mkString(""),
      gameState.drop(3).take(3).map(renderCell).mkString(""),
      gameState.drop(6).take(3).map(renderCell).mkString(""))
  }

  def displayGameHistory(gameHistory: Seq[Seq[Int]]): Unit = {
    val renderedGames = gameHistory.map(renderGame)
    println("")
    println(renderedGames.map(_(0)).mkString("        "))
    println(renderedGames.map(_(1)).mkString("        "))
    println(renderedGames.map(_(2)).mkString("        "))
    println("")

  }

  def noopTrainer(data: RobotTrainingData) = data.player

  val noopTrainer2 = trainRobot { info => info.oldWeight} _

  def simpleTrainer(data: RobotTrainingData) = {
    var robot = data.player.withDefaultValue(1.0)
    (data.won, data.lost) match {
      case (true, false) =>
        for ((state, move) <- data.moveHistory) {
          robot = robot.updated((state, move), robot((state, move)) * 0.5)
        }
      case (false, true) =>
        for ((state, move) <- data.moveHistory) {
          robot = robot.updated((state, move), robot((state, move)) + 1.0)
        }
      case _ =>
        data.player
    }
    robot
  }

  val simpleTrainer2 = trainRobot { info =>
    (info.won, info.lost) match {
      case (true, false) => info.oldWeight + 1
      case (false, true) => info.oldWeight * 0.5
      case _ => info.oldWeight
    }
  } _


  case class MoveInfo(gameLength: Int, moveNumber: Int, won: Boolean, lost: Boolean, oldWeight: Double)

  type WeightUpdater = MoveInfo => Double

  def trainRobot(updater: WeightUpdater)(data: RobotTrainingData) = {
    var robot = data.player.withDefaultValue(1.0)
    val gameLength = data.moveHistory.size
    for (((state, move), moveNum) <- data.moveHistory.zipWithIndex) {
      val info = MoveInfo(gameLength, moveNum, data.won, data.lost, robot((state, move)))
      val newWeight = updater(info)
      robot = robot.updated((state, move), newWeight)
    }
    robot
  }

  def status(msg: String) = jQuery("#status").text(msg)
}
  
