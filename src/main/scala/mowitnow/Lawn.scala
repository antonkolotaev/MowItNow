package mowitnow

class Lawn(width : Int, height : Int)
{
    def isMoveAllowed(position: Position, newPosition : Position) =
    {
        assert(this contains position)
        this contains newPosition
    }

    def contains(position : Position) =
        0 <= position.x && position.x < width && 0 <= position.y && position.y < height
}
