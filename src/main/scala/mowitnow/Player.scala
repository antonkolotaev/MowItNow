package mowitnow

import scala.util.{Failure, Success, Try}

object Player
{
    case class Def(position : Position, orientation : Orientation, commands : Seq[Command])

    case class OutOfLawn(position : Position) extends Throwable { override def toString = s"initial mower position is out of lawn $position"}
    case class DuplicatePosition(position: Position) extends Throwable {override def toString = s"several mowers have same positions $position"}

    def independent(upperRight : Position, mowers : Seq[Def]) : Try[Seq[(Position, Orientation)]] =
    {
        val lawn = new Lawn(upperRight.x + 1, upperRight.y + 1)

        mowers find (m => !(lawn contains m.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.position))
            case None =>
                Success(mowers map { mower =>
                    mower.commands.foldLeft(LawnMower(lawn.isMoveAllowed, mower.position, mower.orientation)) { _ handle _ }
                } map { mower =>
                    (mower.position, mower.orientation)
                })
        }
    }

    def sequential(upperRight : Position, mowers : Seq[Def]) : Try[Seq[(Position, Orientation)]] =
    {
        val lawn = new Lawn(upperRight.x + 1, upperRight.y + 1)

        mowers find (m => !(lawn contains m.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.position))
            case None =>
                mowers.foldLeft(Success(Set.empty[Position]) : Try[Set[Position]]) {
                    case (Success(ps), m) =>
                        if (ps contains m.position)
                            Failure(DuplicatePosition(m.position))
                        else
                            Success(ps + m.position)
                    case (f@Failure(_),_) => f

                } map { positions =>

                        mowers.foldLeft((positions, Set.empty[Position], List.empty[(Position, Orientation)])) {

                            case ((unprocessedPositions, processedPositions, processedMowers), mower) =>

                                val unprocessedPositionsWithoutMe = unprocessedPositions - mower.position

                                def positionAllowed(position: Position, newPosition : Position) = {
                                    lawn.isMoveAllowed(position, newPosition) &&
                                        !(unprocessedPositionsWithoutMe contains newPosition) &&
                                        !(processedPositions contains newPosition)
                                }

                                val newMower = mower.commands.foldLeft(LawnMower(positionAllowed, mower.position, mower.orientation)) {
                                    _ handle _
                                }

                                (unprocessedPositionsWithoutMe,
                                    processedPositions + newMower.position,
                                    (newMower.position, newMower.orientation) :: processedMowers)

                        }._3.reverse
                }
        }


    }

}
