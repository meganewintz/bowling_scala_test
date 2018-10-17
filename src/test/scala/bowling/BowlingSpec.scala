package bowling

import bowling.Bowling.addFrame
import org.scalatest.{FunSpec, Matchers}

import scala.annotation.tailrec

class BowlingSpec extends FunSpec with Matchers {

    describe("For a game") {

        it("all roll with 0 pin down given a score of 0") {
            gameZeroPin shouldEqual 0
        }

        it("all roll with 1 pin down give 20") {
            gameOnePin shouldEqual 20
        }

        it("all roll with 5 pins down give 150") {
            gameFivePins shouldEqual 150
        }

        it("all roll with strikes give 300") {
            gameAllStrike shouldEqual 300
        }

        @tailrec
        def time(game: GameState, pin: Int, numberOfTimes: Int): Int = {
            val frame = Frame(pin, Some(pin), None)

            if(numberOfTimes == 0) game.score
            else {
                val newGame = addFrame(game, frame)

                time(newGame, pin, numberOfTimes - 1)
            }
        }

        @tailrec
        def timeStrike(game: GameState, numberOfTimes: Int): Int = {

            if(numberOfTimes == 0) game.score

            else if (numberOfTimes == 1) {
                val frame = Frame(10, Some(10), Some(10))
                val newGame = addFrame(game, frame)
                timeStrike(newGame, numberOfTimes - 1)
            }
            else {
                val frame = Frame(10, None, None)
                val newGame = addFrame(game, frame)

                timeStrike(newGame, numberOfTimes - 1)
            }
        }

        @tailrec
        def timeSpare(game: GameState, pin: Int, numberOfTimes: Int): Int = {
            val frame = Frame(pin, Some(pin), None)

            if(numberOfTimes == 0) game.score

            else if (numberOfTimes == 1) {
                val frame = Frame(5, Some(5), Some(5))
                val newGame = addFrame(game, frame)
                timeSpare(newGame, pin, numberOfTimes - 1)
            }
            else {
                val newGame = addFrame(game, frame)
                timeSpare(newGame, pin, numberOfTimes - 1)
            }
        }

        def gameZeroPin: Int = time(GameState(0, rollState.EMPTY, rollState.EMPTY), 0,  10)
        def gameOnePin: Int = time(GameState(0, rollState.EMPTY, rollState.EMPTY), 1,  10)
        def gameFivePins: Int = timeSpare(GameState(0, rollState.EMPTY, rollState.EMPTY), 5,  10)
        def gameAllStrike: Int = timeStrike(GameState(0, rollState.EMPTY, rollState.EMPTY), 10)



    }


}
