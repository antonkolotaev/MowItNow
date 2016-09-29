package mowitnow

import scala.util.{Failure, Success, Try}

object Player
{
    case class Task(mower: Mower, commands : Seq[Command])

    case class OutOfLawn(position : Position) extends Throwable { override def toString = s"initial mower position is out of lawn $position"}
    case class DuplicatePosition(position: Position) extends Throwable {override def toString = s"several mowers have same positions $position"}

    def independent(lawn: Lawn, tasks : Seq[Task]) : Try[Seq[Mower]] =
    {
        for (_ <- checkBounds(lawn, tasks))
            yield tasks map { mower =>
                    mower.commands.foldLeft(mower.mower) {
                        (mower, command) => mower handle(command, lawn.isMoveAllowed)
                    }
            }
    }

    def checkBounds(lawn: Lawn, tasks : Seq[Task]) : Try[Unit] =
    {
        tasks find (task => !(lawn contains task.mower.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.mower.position))
            case None => Success(())
        }
    }

    def sequential(lawn: Lawn, tasks : Seq[Task]) : Try[Seq[Mower]] =
    {
        tasks find (task => !(lawn contains task.mower.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.mower.position))
            case None =>
                tasks.foldLeft(Success(Set.empty[Position]) : Try[Set[Position]]) {
                    case (Success(ps), m) =>
                        if (ps contains m.mower.position)
                            Failure(DuplicatePosition(m.mower.position))
                        else
                            Success(ps + m.mower.position)
                    case (f@Failure(_),_) => f

                } map { positions =>

                        tasks.foldLeft((positions, List.empty[Mower])) {

                            case ((ps, processedMowers), task) =>

                                val positionsWithoutMe = ps - task.mower.position

                                def positionAllowed(position: Position, newPosition : Position) = {
                                    lawn.isMoveAllowed(position, newPosition) && !(positionsWithoutMe contains newPosition)
                                }

                                val newMower = task.commands.foldLeft(task.mower) {
                                    (mower, command) => mower handle (command, positionAllowed)
                                }

                                (positionsWithoutMe + newMower.position, newMower :: processedMowers)

                        }._2.reverse
                }
        }


    }

    def concurrent(lawn: Lawn, tasks : Seq[Task], positions : Set[Position]) : Seq[Task] =
    {
        val (newTasks, newPositions) =
            tasks.foldLeft((List.empty[Task], positions)) {
                case ((result, forbidden), task) =>

                    task.commands match {
                        case Nil => (task :: result, forbidden)
                        case currentCommand :: commandsToProcess =>
                            val forbiddenWithoutMe = forbidden - task.mower.position

                            def positionAllowed(position: Position, newPosition : Position) = {
                                lawn.isMoveAllowed(position, newPosition) && !(forbiddenWithoutMe contains newPosition)
                            }

                            val newMower = task.mower handle (currentCommand, positionAllowed)

                            (Task(newMower, commandsToProcess) :: result, forbiddenWithoutMe + newMower.position)
                    }

            }

        if (newTasks forall { _.commands.isEmpty })
            newTasks.reverse
        else
            concurrent(lawn, newTasks.reverse, newPositions)
    }


    def concurrent(lawn: Lawn, tasks : Seq[Task]) : Try[Seq[Mower]] =
    {
        tasks find (task => !(lawn contains task.mower.position)) match {
            case Some(outlier) => Failure(OutOfLawn(outlier.mower.position))
            case None =>
                tasks.foldLeft(Success(Set.empty[Position]): Try[Set[Position]]) {
                    case (Success(ps), task) =>
                        if (ps contains task.mower.position)
                            Failure(DuplicatePosition(task.mower.position))
                        else
                            Success(ps + task.mower.position)
                    case (f@Failure(_), _) => f

                } map { positions =>
                    concurrent(lawn, tasks, positions) map { _.mower }
                }
        }
    }

}
