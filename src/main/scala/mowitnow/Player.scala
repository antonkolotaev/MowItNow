package mowitnow

import scala.util.{Failure, Success, Try}

object Player
{
    /**
     * Input definition for a mower
     * @param mower - initial position and orientation of mower
     * @param commands - commands to be played
     */
    case class Task(mower: Mower, commands : Seq[Command])

    /**
     * Error class signaling that initial position of a mower is out of lawn
     * @param position incorrect mower position
     */
    case class OutOfLawn(position : Position) extends Throwable
    {
        override def toString = s"initial mower position is out of lawn $position"
    }

    /**
     * Error class signaling that two mowers have the same initial positions
     * @param position incorrect mower position
     */
    case class DuplicatePosition(position: Position) extends Throwable
    {
        override def toString = s"several mowers have same positions $position"
    }

    /**
     * Moves mowers absolutely independently without any respect to other mowers
     * @param lawn lawn where mowers to be moved
     * @param tasks mowers and their commands
     * @return Success(final position and orientation for every mower); Failure(_) otherwise
     */
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

    private def sequentialImpl(lawn: Lawn, tasks : Seq[Task], positions : Set[Position]) : Seq[Mower] =
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

    /**
     * Moves mowers sequentially:
     *  1. Put all mowers into the lawn
     *  2. Check that all have different positions and these positions are inside of the lawn
     *  3. Play commands for i-th (i in 0..tasks.length) mower taking into account positions of other mowers
     *
     * @param lawn lawn where mowers to be moved
     * @param tasks mowers and their commands
     * @return Success(final position and orientation for every mower); Failure(_) otherwise
     */
    def sequential(lawn: Lawn, tasks : Seq[Task]) : Try[Seq[Mower]] =
    {
        for (_         <- checkBounds(lawn, tasks);
             positions <- checkDuplicates(positionsOf(tasks))
        )
            yield sequentialImpl(lawn, tasks, positions)
    }

    private def concurrentImpl(lawn: Lawn, tasks : Seq[Task], positions : Set[Position]) : Seq[Task] =
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
            concurrentImpl(lawn, newTasks.reverse, newPositions)
    }


    /**
     * Moves mowers concurrently:
     *  1. Put all mowers into the lawn
     *  2. Check that all have different positions and these positions are inside of the lawn
     *  3. Play the first command for i-th (i in 0..tasks.length) mower taking into account positions of other mowers
     *  4. Repeat step 3 until all mowers have no commands to replay
     *
     * @param lawn lawn where mowers to be moved
     * @param tasks mowers and their commands
     * @return Success(final position and orientation for every mower); Failure(_) otherwise
     */
    def concurrent(lawn: Lawn, tasks : Seq[Task]) : Try[Seq[Mower]] =
    {
        for (_         <- checkBounds(lawn, tasks);
             positions <- checkDuplicates(positionsOf(tasks))
        )
            yield concurrentImpl(lawn, tasks, positions) map { _.mower }
    }

}
