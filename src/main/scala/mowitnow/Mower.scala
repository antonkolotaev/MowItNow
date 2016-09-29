package mowitnow

case class Mower(position: Position, orientation: Orientation)
{
    def left  = copy(orientation = orientation.left)
    def right = copy(orientation = orientation.right)
    def forward(isMoveAllowed : (Position, Position) => Boolean) =
        copy(position = {
            val newPosition = position treat orientation
            if (isMoveAllowed (position, newPosition))
                newPosition
            else
                position
        })

    def handle(command : Command, isMoveAllowed : (Position, Position) => Boolean) =
        command match {
            case Left    => left
            case Right   => right
            case Forward => forward(isMoveAllowed)
        }
}

