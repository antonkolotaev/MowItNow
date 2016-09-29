package mowitnow

import scala.util.parsing.combinator._

trait Parser extends JavaTokenParsers with PackratParsers {

    override val skipWhitespace = false

    def whitespace = """[ \t]+""".r
    def whitespaceNL = """[ \t]*\n""".r
    def toStrip = """[ \t\n]*""".r

    def integer : Parser[Int]    = ("""([0-9]\d*)""".r ^^ { _.toInt }) withFailureMessage "integer expected"

    def position = (((opt(whitespace) ~> integer <~ whitespace) ~ integer) ^^ { case x ~ y  => Position(x,y) }
        ) withFailureMessage "position expected"

    def dimensions : Parser[Position] = position <~ whitespaceNL

    def east  = "E" ^^ { _ => Orientation.East }
    def north = "N" ^^ { _ => Orientation.North }
    def west  = "W" ^^ { _ => Orientation.West }
    def south = "S" ^^ { _ => Orientation.South }

    def orientation = (east | north | west | south) withFailureMessage "orientation expected"

    def initial = ((position <~ whitespace) ~ (orientation <~ whitespaceNL) ^^ { case pos ~ orient => Mower(pos,orient) }
        ) withFailureMessage "initial mower position and orientation expected"

    def forward = "A" ^^ { _ => Forward }
    def left    = "G" ^^ { _ => Left }
    def right   = "D" ^^ { _ => Right }

    def command = (forward | left | right) withFailureMessage "command expected"

    def commands = opt(whitespace) ~> rep(command) <~ opt(whitespace)

    def mower = ((initial ~ commands) ^^ {case mower ~ cmds => Player.Def(mower, cmds)}
        ) withFailureMessage "mower definition expected"

    def all = (toStrip ~> (dimensions ~ repsep(mower, "\n") <~ toStrip) ^^ { case dims ~ mowers => (dims, mowers) }
        ) withFailureMessage "game definition expected"
}
