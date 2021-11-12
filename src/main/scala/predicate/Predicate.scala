package object predicate {
  trait Predicate[T] extends Function[T, Boolean] {
    def or(p2: Predicate[T]) = OrPredicate[T](this, p2)
    def and(p2: Predicate[T]) = AndPredicate[T](this, p2)
    def nand(p2: Predicate[T]) = NandPredicate(this, p2)
    def nor(p2: Predicate[T]) = NorPredicate(this, p2)
    def xor(p2: Predicate[T]) = XorPredicate(this, p2)
    def unary_! = NotPredicate[T](this)
  }
  case class OrPredicate[T](p1: Predicate[T], p2: Predicate[T]) extends Predicate[T] {
    def apply(a: T) = p1(a) || p2(a)
  }

  case class AndPredicate[T](p1: Predicate[T], p2: Predicate[T]) extends Predicate[T] {
    def apply(a: T) = p1(a) && p2(a)
  }

  case class NotPredicate[T](p: Predicate[T]) extends Predicate[T] {
    def apply(a: T) = !p(a)
  }

  case class NandPredicate[T](p1: Predicate[T], p2: Predicate[T])  {
    def apply(a: T) = !(p1(a) && p2(a))
  }
  case class NorPredicate[T](p1: Predicate[T], p2: Predicate[T])  {
    def apply(a: T) = !(p1(a) || p2(a))
  }
  case class XorPredicate[T](p1: Predicate[T], p2: Predicate[T])  {
    def apply(a: T) = (p1(a) || p2(a)) && !(p1(a) && p2(a))
  }
 case class TruePredicate[T](r:T) extends Predicate[T] {
    def apply(a: T) = true
  }
  case class FalsePredicate[T](r:T) extends Predicate[T] {
    def apply(a: T) = false
  }
}
