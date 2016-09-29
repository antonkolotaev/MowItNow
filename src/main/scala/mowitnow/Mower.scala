package mowitnow

/**
 * Represents an immutable mower
 * @param position - mower position
 * @param orientation - mower orientation
 */
case class Mower(position: Position, orientation: Orientation)
{
    /**
     * @return Mower copy turned to left
     */
    def left  = copy(orientation = orientation.left)

    /**
     * @return Mower copy turned to right
     */
    def right = copy(orientation = orientation.right)

    /**
     * Tries to make a step forward
     * @param isMoveAllowed - function to determine if a move is allowed
     * @return Mower copy after the move
     */
    def forward(isMoveAllowed : (Position, Position) => Boolean) =
        copy(position = {
            val newPosition = position treat orientation
            if (isMoveAllowed (position, newPosition))
                newPosition
            else
                position
        })

    /**
     * Handles generic command
     * @param command Command: Left, Right or Forward
     * @param isMoveAllowed - function to determine if a move is allowed
     * @return mower copy after handling the command
     */
    def handle(command : Command, isMoveAllowed : (Position, Position) => Boolean) =
        command match {
            case Left    => left
            case Right   => right
            case Forward => forward(isMoveAllowed)
        }
}

