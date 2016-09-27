package object mowitnow {

    case class Orientation(i : Int)
    {
        def left = Orientation((i + 3) % 4)
        def right = Orientation((i + 1) % 4)

        override def toString = i match {
            case 0 => "N"
            case 1 => "E"
            case 2 => "S"
            case 3 => "W"
        }
    }

    object Orientation
    {
        val North = Orientation(0)
        val East = Orientation(1)
        val South = Orientation(2)
        val West = Orientation(3)
    }


    case class Position(x : Int, y : Int)
    {
        def treat(orientation: Orientation) =
            orientation match {
                case Orientation.North => copy(x, y + 1)
                case Orientation.West  => copy(x - 1, y)
                case Orientation.East  => copy(x + 1, y)
                case Orientation.South => copy(x, y - 1)
            }
        override def toString = s"($x,$y)"
    }

    sealed trait Command

    case object Forward extends Command
    case object Left extends Command
    case object Right extends Command


}
