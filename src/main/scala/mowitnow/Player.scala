package mowitnow

import scala.util.{Failure, Success, Try}

object Player
{
    case class Def(mower: Mower, commands : Seq[Command])

    case class OutOfLawn(position : Position) extends Throwable { override def toString = s"initial mower position is out of lawn $position"}
    case class DuplicatePosition(position: Position) extends Throwable {override def toString = s"several mowers have same positions $position"}

    def independent(lawn: Lawn, mowers : Seq[Def]) : Try[Seq[Mower]] =
    {
        for (_ <- checkBounds(lawn, mowers))
            yield mowers map { mower =>
                    mower.commands.foldLeft(mower.mower) {
                        (mower, command) => mower handle(command, lawn.isMoveAllowed)
                    }
            }
    }

    def checkBounds(lawn: Lawn, mowers : Seq[Def]) : Try[Unit] =
    {
        mowers find (m => !(lawn contains m.mower.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.mower.position))
            case None => Success(())
        }
    }

    def sequential(lawn: Lawn, mowers : Seq[Def]) : Try[Seq[Mower]] =
    {
        mowers find (m => !(lawn contains m.mower.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.mower.position))
            case None =>
                mowers.foldLeft(Success(Set.empty[Position]) : Try[Set[Position]]) {
                    case (Success(ps), m) =>
                        if (ps contains m.mower.position)
                            Failure(DuplicatePosition(m.mower.position))
                        else
                            Success(ps + m.mower.position)
                    case (f@Failure(_),_) => f

                } map { positions =>

                        mowers.foldLeft((positions, List.empty[Mower])) {

                            case ((ps, processedMowers), mower) =>

                                val positionsWithoutMe = ps - mower.mower.position

                                def positionAllowed(position: Position, newPosition : Position) = {
                                    lawn.isMoveAllowed(position, newPosition) && !(positionsWithoutMe contains newPosition)
                                }

                                val newMower = mower.commands.foldLeft(mower.mower) {
                                    (mower, command) => mower handle (command, positionAllowed)
                                }

                                (positionsWithoutMe + newMower.position, newMower :: processedMowers)

                        }._2.reverse
                }
        }


    }

    def concurrent(lawn: Lawn, mowers : Seq[Def], positions : Set[Position]) : Seq[Def] =
    {
        val (newMowers, newPositions) =
            mowers.foldLeft((List.empty[Def], positions)) {
                case ((result, forbidden), mower) =>

                    mower.commands match {
                        case Nil => (mower :: result, forbidden)
                        case hd :: tl =>
                            val forbiddenWithoutMe = forbidden - mower.mower.position

                            def positionAllowed(position: Position, newPosition : Position) = {
                                lawn.isMoveAllowed(position, newPosition) && !(forbiddenWithoutMe contains newPosition)
                            }

                            val newMower = mower.mower handle (hd, positionAllowed)

                            (Def(newMower, tl) :: result, forbiddenWithoutMe + newMower.position)
                    }

            }

        if (newMowers forall { _.commands.isEmpty })
            newMowers.reverse
        else
            concurrent(lawn, newMowers.reverse, newPositions)
    }


    def concurrent(lawn: Lawn, mowers : Seq[Def]) : Try[Seq[Mower]] =
    {
        mowers find (m => !(lawn contains m.mower.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.mower.position))
            case None =>
                mowers.foldLeft(Success(Set.empty[Position]): Try[Set[Position]]) {
                    case (Success(ps), m) =>
                        if (ps contains m.mower.position)
                            Failure(DuplicatePosition(m.mower.position))
                        else
                            Success(ps + m.mower.position)
                    case (f@Failure(_), _) => f

                } map { positions =>
                    concurrent(lawn, mowers, positions) map { mower => mower.mower }
                }
        }
    }

}
