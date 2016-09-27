package mowitnow

import org.scalatest._

class OrientationSpec extends FlatSpec with Matchers {

    "North" should " be East after Right" in { Orientation.North.right should be (Orientation.East) }
    it      should " be West after Left" in { Orientation.North.left should be (Orientation.West) }

    "East"  should " be South after Right" in { Orientation.East.right should be (Orientation.South) }
    it      should " be North after Left" in { Orientation.East.left should be (Orientation.North) }

    "South" should " be West after Right" in { Orientation.South.right should be (Orientation.West) }
    it      should " be East after Left" in { Orientation.South.left should be (Orientation.East) }

    "West"  should " be North after Right" in { Orientation.West.right should be (Orientation.North) }
    it      should " be South after Left" in { Orientation.West.left should be (Orientation.South) }

}
