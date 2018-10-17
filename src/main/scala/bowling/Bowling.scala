package bowling

object rollState extends Enumeration {
    val REGULAR  = Value
    val STRIKE  = Value
    val SPARE   = Value
    val EMPTY   = Value
}

case class GameState(score: Int, previousRollState1: rollState.Value, previousRollState2: rollState.Value)

case class Frame(firstRoll: Int, secondRoll: Option[Int], thirdRoll: Option[Int])

object Bowling {


    def addFrame(game: GameState, frame: Frame): GameState = {

        val previousScore = calculatePreviousScore(game.score, frame, game.previousRollState1, game.previousRollState2)

        if (frame.thirdRoll.isDefined) {
            game.copy(score = previousScore + frame.firstRoll + frame.secondRoll.getOrElse(0) + frame.thirdRoll.getOrElse(0), previousRollState1 = rollState.STRIKE, previousRollState2 = game.previousRollState1)
        }
        else if (frame.firstRoll == 10) {
            game.copy(score = previousScore + frame.secondRoll.getOrElse(0) + frame.thirdRoll.getOrElse(0), previousRollState1 = rollState.STRIKE, previousRollState2 = game.previousRollState1)
        }
        else if(frame.firstRoll + frame.secondRoll.getOrElse(0) == 10) {
            game.copy(score = previousScore + frame.firstRoll + frame.secondRoll.getOrElse(0) + frame.thirdRoll.getOrElse(0), previousRollState1 = rollState.SPARE, previousRollState2 = game.previousRollState1)
        }
        else {
            val newScore = previousScore + frame.firstRoll + frame.secondRoll.getOrElse(0)
            game.copy(score = newScore, previousRollState1 = rollState.REGULAR, previousRollState2 = game.previousRollState1)
        }
    }


    def calculatePreviousScore(currentScore: Int, currentRoll: Frame, previousRollState1: rollState.Value, previousRollState2: rollState.Value): Int = {
        previousRollState1 match {
            case rollState.EMPTY | rollState.REGULAR => currentScore
            case rollState.SPARE => currentScore + currentRoll.firstRoll
            case rollState.STRIKE => {
                previousRollState2 match {
                    case rollState.EMPTY => currentScore
                    case rollState.STRIKE => currentScore + 20 + currentRoll.firstRoll + currentRoll.secondRoll.getOrElse(0) + currentRoll.thirdRoll.getOrElse(0) +  + currentRoll.thirdRoll.getOrElse(0)
                    case _ => currentScore + 10 + currentRoll.firstRoll + currentRoll.secondRoll.getOrElse(0) + currentRoll.thirdRoll.getOrElse(0)

                }
            }
        }
    }


}