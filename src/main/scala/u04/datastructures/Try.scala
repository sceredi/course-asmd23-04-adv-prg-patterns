package u04.datastructures

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll
import Sequences.*
import Sequence.*

@main def a =
    def intSequence: Gen[Sequence[Int]] = for
        i <- Gen.choose(0, 100)
        b <- Gen.prob(0.8)
        s <- if b then intSequence.map(s2 => Cons(i, s2)) else Gen.const(Nil())
    yield s
    Range(0,100).foreach(i => println(intSequence.sample))


