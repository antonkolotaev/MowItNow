package mowitnow

class IndependentSpec extends BaseSpec(Player.independent) {

    "independent mowers" should "conform to the assignment spec" in assignmentSpec

    it should "report about incorrect mowers" in outOfLawnSpec
}
