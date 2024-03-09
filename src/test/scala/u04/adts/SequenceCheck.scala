package u04.adts

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

import u04.datastructures.*
import Sequences.*
import Sequence.*

object SequenceCheck extends Properties("Sequence"):

  def intSequenceGen(): Gen[Sequence[Int]] = for
    i <- Gen.choose(0, 100)
    b <- Gen.prob(0.8)
    s <- if b then intSequenceGen().map(s2 => Cons(i, s2)) else Gen.const(Nil())
  yield s

  def mapperGen(): Gen[Int => Int] = Gen.oneOf[Int => Int]( _+1, _*2, x => x*x )

  property("of is a correct factory") =
    forAll(Gen.choose(0,100), arbitrary[String]): (i, s) =>
      of(i, s) == of(i, s).filter(e => e == s)
    &&
    forAll(Gen.choose(0, 100), arbitrary[String]): (i, s) =>
      of(i, s).filter(e => e != s) == Nil()
    &&
    forAll(Gen.choose(0, 100), arbitrary[String]): (i, s) =>
      Cons(s, of(i, s)) == of(i+1, s)
    &&
    forAll(arbitrary[String]): s =>
      of(0, s) == Nil()

  property("mapAxioms") =
    forAll(intSequenceGen(), mapperGen()):
      case (Nil(), f) =>  map(Nil())(f) == Nil()
      case (Cons(h, t), f) => map(Cons(h, t))(f) == Cons(f(h), map(t)(f))

