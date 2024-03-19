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

  property("map composition") =
    forAll(sequenceGen[Int], ((el: Int) => el + 1), ((el: Int) => el * 2)) {
      (seq, f, g) =>
        seq.map(f).map(g) == seq.map(f andThen g)
    }

  property("map general") = forAll(sequenceGen[Int], ((el: Int) => el + 1)) {
    (seq, f) =>
      val other = uncons(seq) match
        case Some((h, t)) => cons(f(h), t.map(f))
        case None         => nil()
      seq.map(f) == other
  }

  property("map flatmap similarity") = forAll(sequenceGen[Int]) { seq =>
    seq.map(_ + 1) == seq.flatMap(el => cons(el + 1, nil()))
  }

  property("flatMap associativity") = forAll(
    sequenceGen[Int],
    ((el: Int) => cons(el + 1, nil())),
    ((el: Int) => cons(el * 2, nil()))
  ) { (seq, f, g) =>
    seq.flatMap(f).flatMap(g) == seq.flatMap(el => f(el).flatMap(g))
  }

  property("flatMap general") =
    forAll(sequenceGen[Int], ((el: Int) => cons(el + 1, nil()))) { (seq, f) =>
      val other = uncons(seq) match
        case Some((h, t)) => concat(f(h), t.flatMap(f))
        case None         => nil()
      seq.flatMap(f) == other
    }

  property("filter identity") = forAll(sequenceGen[Int]) { seq =>
    seq.filter(_ => true) == seq
  }

  property("filter general") =
    forAll(sequenceGen[Int], ((el: Int) => el % 2 == 0)) { (seq, p) =>
      val other = uncons(seq) match
        case Some((h, t)) if p(h) => cons(h, t.filter(p))
        case Some((_, t))         => t.filter(p)
        case None                 => nil()
      seq.filter(p) == other
    }
