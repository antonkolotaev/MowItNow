package mowitnow

case class LawnMower(isMoveAllowed : (Position, Position) => Boolean, position: Position, orientation: Orientation)
{
    def left  = copy(orientation = orientation.left)
    def right = copy(orientation = orientation.right)
    def forward = copy(position = {
        val newPosition = position treat orientation
        if (isMoveAllowed (position, newPosition))
            newPosition
        else
            position
    })

    def handle(command : Command) =
        command match {
            case Left    => left
            case Right   => right
            case Forward => forward
        }
}

