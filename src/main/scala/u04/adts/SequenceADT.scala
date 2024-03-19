package u04.adts

object SequenceADT:
  // implementation of data structure: fully hidden
  private enum SequenceImpl[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()
  import SequenceImpl.*

  // the type Sequence is an opaque alias to SequenceImpl
  opaque type Sequence[A] = SequenceImpl[A]

  // constructors
  def cons[E](head: E, tail: Sequence[E]): Sequence[E] = Cons(head, tail)
  def nil[E](): Sequence[E] = Nil[E]()

  // destructors
  def uncons[E](seq: Sequence[E]): Option[(E, Sequence[E])] = seq match
    case Cons(h, t) => Some((h, t))
    case Nil()      => None

  // operation as extension methods
  extension [A](seq: Sequence[A])
    def map[B](mapper: A => B): Sequence[B] = seq match
      case Cons(h, t) => Cons(mapper(h), t.map(mapper))
      case Nil()      => Nil()

    def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = seq match
      case Cons(h, t) => concat(mapper(h), t.flatMap(mapper))
      case Nil()      => Nil()

  // operations as standard methods
  def concat[A](seq1: Sequence[A], seq2: Sequence[A]): Sequence[A] = seq1 match
    case Cons(h, t) => Cons(h, concat(t, seq2))
    case Nil()      => seq2

@main def trySequenceADT =
  import SequenceADT.*
  val seq = cons(10, cons(20, cons(30, nil())))
  println(concat(seq, seq.map(_ + 1)))
