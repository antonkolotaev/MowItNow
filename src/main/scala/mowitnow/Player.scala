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
                    mower.commands.foldLeft(LawnMower(mower.position, mower.orientation)) {
                        (mower, command) => mower handle (command, lawn.isMoveAllowed)
                    }
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

                        mowers.foldLeft((positions, List.empty[(Position, Orientation)])) {

                            case ((ps, processedMowers), mower) =>

                                val positionsWithoutMe = ps - mower.position

                                def positionAllowed(position: Position, newPosition : Position) = {
                                    lawn.isMoveAllowed(position, newPosition) && !(positionsWithoutMe contains newPosition)
                                }

                                val newMower = mower.commands.foldLeft(LawnMower(mower.position, mower.orientation)) {
                                    (mower, command) => mower handle (command, positionAllowed)
                                }

                                (positionsWithoutMe + newMower.position,
                                    (newMower.position, newMower.orientation) :: processedMowers)

                        }._2.reverse
                }
        }


    }

    def concurrent(lawn: Lawn, mowers : Seq[Def]) : Seq[(Position, Orientation)] =
    {
        mowers map { mower => (mower.position, mower.orientation) }
    }


    def concurrent(upperRight : Position, mowers : Seq[Def]) : Try[Seq[(Position, Orientation)]] =
    {
        val lawn = new Lawn(upperRight.x + 1, upperRight.y + 1)

        mowers find (m => !(lawn contains m.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.position))
            case None =>
                mowers.foldLeft(Success(Set.empty[Position]): Try[Set[Position]]) {
                    case (Success(ps), m) =>
                        if (ps contains m.position)
                            Failure(DuplicatePosition(m.position))
                        else
                            Success(ps + m.position)
                    case (f@Failure(_), _) => f

                } map { positions =>
                    concurrent(lawn, mowers)
                }
        }
    }

}
