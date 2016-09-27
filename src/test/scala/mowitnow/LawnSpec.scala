package mowitnow

import org.scalatest._

class LawnSpec extends FlatSpec with Matchers {

    "A lawn" should "allow to move inside it" in {
        new Lawn(5,3) isMoveAllowed (Position(0,0), Position(0,1)) should be (true)
    }

    it should "disallow moves with x less than 0" in {
        new Lawn(5,3) isMoveAllowed (Position(0,0), Position(-1,0)) should be (false)
    }

    it should "disallow moves with y less than 0" in {
        new Lawn(5,3) isMoveAllowed (Position(0,0), Position(0,-1)) should be (false)
    }

    it should "disallow moves with x greater or equal than width" in {
        new Lawn(5,3) isMoveAllowed (Position(4,0), Position(5,0)) should be (false)
    }

    it should "disallow moves with y greater or equal than height" in {
        new Lawn(5,3) isMoveAllowed (Position(0,2), Position(0,3)) should be (false)
    }

}
