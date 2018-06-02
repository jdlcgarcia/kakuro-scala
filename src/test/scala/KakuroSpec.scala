import Kakuro._
import org.scalatest._

class KakuroSpec extends FlatSpec with Matchers {

  "Approach1" should "be able to find the list of combinations" in {

    val approach1 = ApproachInline(10, 3)
    val approach2 = ApproachInline(24, 4)
    val approach3 = ApproachInline(11, 4)
    val approach4 = ApproachInline(15, 3)

    approach1.comb shouldBe List(
      List(1, 2, 7),
      List(1, 3, 6),
      List(1, 4, 5),
      List(2, 3, 5))

    approach2.comb shouldBe List(
      List(1, 6, 8, 9),
      List(2, 5, 8, 9),
      List(2, 6, 7, 9),
      List(3, 4, 8, 9),
      List(3, 5, 7, 9),
      List(3, 6, 7, 8),
      List(4, 5, 6, 9),
      List(4, 5, 7, 8))

    approach3.comb shouldBe List(List(1, 2, 3, 5))

    approach4.comb shouldBe List(
      List(1, 5, 9),
      List(1, 6, 8),
      List(2, 4, 9),
      List(2, 5, 8),
      List(2, 6, 7),
      List(3, 4, 8),
      List(3, 5, 7),
      List(4, 5, 6))

  }

  "Approach2" should "be able to find the list of combinations" in {

    val approach1 = Spaces(10, 3, Set(2))
    val approach2 = Spaces(24, 4, Set(3, 8))
    val approach3 = Spaces(11, 4)
    val approach4 = Spaces(15, 3, Set(7, 2))

    approach1.comb shouldBe List(
      List(1, 2, 7),
      List(2, 3, 5))

    approach2.comb shouldBe List(
      List(3, 4, 8, 9),
      List(3, 6, 7, 8))

    approach3.comb shouldBe List(List(1, 2, 3, 5))

    approach4.comb shouldBe List(List(2, 6, 7))

  }


}