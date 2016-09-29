package mowitnow

import scala.util.{Success, Failure}

class SequentialSpec extends BaseSpec(Player.sequential) {

    "sequential mowers" should "conform to the assignment spec" in assignmentSpec

    it should "report about incorrect mowers" in outOfLawnSpec

    it should "report about duplicate mowers" in duplicateSpec

    it should "bump into unprocessed mowers" in {

        Player.sequential(
            Lawn fromUpperRight Position(5,5), List(
                Player.Task(Mower(Position(1,2), Orientation.North),
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Task(Mower(Position(0,1), Orientation.East),
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List(Mower(Position(1,4), Orientation.North), Mower(Position(2,0), Orientation.East))
            case Failure(_) =>
                assert(false)

        }
    }

    it should "bump into processed mowers" in {

        Player.sequential(
            Lawn fromUpperRight Position(5,5), List(
                Player.Task(Mower(Position(0,1), Orientation.East),
                    List()),
                Player.Task(Mower(Position(1,2), Orientation.North),
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)))
        ) match {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List(Mower(Position(0,1), Orientation.East), Mower(Position(1,4), Orientation.North))
            case Failure(_) =>
                assert(false)
        }
    }

}
