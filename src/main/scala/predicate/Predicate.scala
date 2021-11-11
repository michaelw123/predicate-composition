package object predicate {
  trait Predicate[T] extends Function[T, Boolean] {
    def or(p2: Predicate[T]) = OrPredicate[T](this, p2)
    def and(p2: Predicate[T]) = AndPredicate[T](this, p2)
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
    def apply(a: T) = !(p1 and p2)
  }
  case class NorPredicate[T](p1: Predicate[T], p2: Predicate[T])  {
    def apply(a: T) = !(p1 or p2)
  }
  case class XorPredicate[T](p1: Predicate[T], p2: Predicate[T])  {
    def apply(a: T) = (p1 or p2) and !(p1 and p2)
  }
 case class TruePredicate[T](r:T) extends Predicate[T] {
    def apply(a: T) = true
  }
  case class FalsePredicate[T](r:T) extends Predicate[T] {
    def apply(a: T) = false
  }
}
