package u04.datastructures

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary.arbitrary
import Sequences.*, Sequence.*
import org.scalacheck.Test
import scala.util.Random

object SequenceCheck extends Properties("Sequence"):
  def smallInt(): Gen[Int] = Gen.choose(0, 100)

  property("of is a correct factory") = forAll(
    smallInt(),
    arbitrary[String]
  ) { (i, s) =>
    of(i, s) == of(i, s).filter(e => e == s) &&
    of(i, s).filter(e => e != s) == Nil() &&
    Cons(s, of(i, s)) == of(i + 1, s) &&
    of(0, s) == Nil()
  }

  // Here I was trying to make it so that even if sometimes it fails it should work at least once
  // property("of also works with integers") = forAll(
  //   smallInt(),
  //   arbitrary[Int],
  //   Test.Parameters.default.withMinSuccessfulTests(1),
  // ) { (i, s, _) =>
  //   of(i, s) == of(i, s).filter(e => Random().nextInt() % 2 == 0) &&
  //   of(i, s).filter(e => e != s) == Nil() &&
  //   Cons(s, of(i, s)) == of(i + 1, s) &&
  //   of(0, s) == Nil()
  // }
