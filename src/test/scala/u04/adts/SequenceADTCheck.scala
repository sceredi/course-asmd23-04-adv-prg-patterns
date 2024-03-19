package scala.u04.adts

import org.scalacheck.{Gen, Properties, Arbitrary}
import org.scalacheck.Gen.const
import org.scalacheck.Prop.forAll
import u04.adts.SequenceADT.*

object SequenceADTCheck extends Properties("sequenceADT"):
  private def nilGen[A]: Gen[Sequence[A]] = const(nil())

  private def consGen[A: Arbitrary]: Gen[Sequence[A]] = for
    head <- Arbitrary.arbitrary[A]
    tail <- Gen.oneOf(nilGen, consGen)
  yield cons(head, tail)

  private def sequenceGen[A: Arbitrary]: Gen[Sequence[A]] =
    Gen.oneOf(nilGen, consGen)

  property("map identity") = forAll(sequenceGen[Int]) { seq =>
    seq.map(el => el) == seq
  }
