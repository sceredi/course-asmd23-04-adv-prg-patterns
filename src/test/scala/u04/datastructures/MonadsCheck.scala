package u04.datastructures

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

// Left Identity (return/unit) law
def leftIdentityLaw[A, B](a: A, f: A => Option[B]): Boolean = {
  Some(a).flatMap(f) == f(a)
}

// Right Identity (bind) law
def rightIdentityLaw[A](m: Option[A]): Boolean = {
  m.flatMap(Some(_)) == m
}

// Associativity law
def associativityLaw[A, B, C](
    m: Option[A],
    f: A => Option[B],
    g: B => Option[C]
): Boolean = {
  m.flatMap(f).flatMap(g) == m.flatMap(f(_).flatMap(g))
}

object MonadsLawsCheck extends Properties("monads"):
  property("leftIdentity") = forAll { (a: Int, f: Int => Option[Int]) =>
    leftIdentityLaw(a, f)
  }
  property("rightIdentity") = forAll { (m: Option[Int]) =>
    rightIdentityLaw(m)
  }
  property("associativity") = forAll {
    (m: Option[Int], f: Int => Option[Int], g: Int => Option[Int]) =>
      associativityLaw(m, f, g)
  }
