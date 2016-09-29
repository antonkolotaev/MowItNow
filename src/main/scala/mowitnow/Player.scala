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
            yield tasks map { task =>
                    task.commands.foldLeft(task.mower) {
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

    def checkDuplicates(positions : Seq[Position]) : Try[Set[Position]] =
    {
        positions.foldLeft(Success(Set.empty[Position]): Try[Set[Position]]) {
            case (Success(ps), position) =>
                if (ps contains position)
                    Failure(DuplicatePosition(position))
                else
                    Success(ps + position)
            case (f@Failure(_), _) => f
        }
    }

    def positionsOf(tasks : Seq[Task]) = tasks map { _.mower.position }

    def sequential(lawn: Lawn, tasks : Seq[Task], positions : Set[Position]) : Seq[Mower] =
    {
        tasks.foldLeft((positions, List.empty[Mower])) {

            case ((ps, processedMowers), task) =>

                val positionsWithoutMe = ps - task.mower.position

                def positionAllowed(position: Position, newPosition: Position) = {
                    lawn.isMoveAllowed(position, newPosition) && !(positionsWithoutMe contains newPosition)
                }

                val newMower = task.commands.foldLeft(task.mower) {
                    (mower, command) => mower handle(command, positionAllowed)
                }

                (positionsWithoutMe + newMower.position, newMower :: processedMowers)

        }._2.reverse
    }

    def sequential(lawn: Lawn, tasks : Seq[Task]) : Try[Seq[Mower]] =
    {
        for (_         <- checkBounds(lawn, tasks);
             positions <- checkDuplicates(positionsOf(tasks))
        )
            yield sequential(lawn, tasks, positions)
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
        for (_         <- checkBounds(lawn, tasks);
             positions <- checkDuplicates(positionsOf(tasks))
        )
            yield concurrent(lawn, tasks, positions) map { _.mower }
    }

}
