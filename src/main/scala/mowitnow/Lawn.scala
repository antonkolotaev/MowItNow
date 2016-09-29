package mowitnow

/**
 * Represents a rectangular lawn
 * @param width - x in [0, width)
 * @param height - y in [0, height)
 */
class Lawn(width : Int, height : Int)
{
    /**
     * Checks whether it is allowed to move from 'position' to 'newPosition'
     * @param position - old mower position
     * @param newPosition - new mower position
     * @return true iff this move is allowed
     */
    def isMoveAllowed(position: Position, newPosition : Position) =
    {
        assert(this contains position)
        this contains newPosition
    }

    /**
     * Determines whether 'position' belongs to the lawn
     * @return true iff 'position' belongs to the lawn
     */
    def contains(position : Position) =
        0 <= position.x && position.x < width && 0 <= position.y && position.y < height
}

object Lawn
{
    /**
     * constructs a rectangular lawn with (0,0) as lower-bottom corner and 'upperRight' as upper-right corner
     */
    def fromUpperRight(upperRight : Position) =
    {
        new Lawn(upperRight.x + 1, upperRight.y + 1)
    }
}
