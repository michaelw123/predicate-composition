import predicate._
import org.scalatest.flatspec.AnyFlatSpec

class Algebra extends AnyFlatSpec{
  implicit class IntEqualTo(mark:Int) extends Predicate[Int] {
    def apply(a:Int) = a == mark
  }
  implicit class IntLessThan(mark:Int) extends Predicate[Int] {
    def apply(a:Int) = a < mark
  }
  implicit class IntGreaterThan(mark:Int) extends Predicate[Int] {
    def apply(a:Int) = a > mark
  }
  implicit class isEvenPredicate(x:Int=0) extends Predicate[Int] {
    def apply(a:Int) = a % 2 == x
  }
  implicit class isOddPredicate(x:Int=1) extends Predicate[Int] {
    def apply(a:Int) = a % 2 == x
  }
  implicit class IntAndIdentityPredicate(x:Int=0) extends  Predicate[Int]{
    def apply(a:Int) = true
  }
  implicit class IntOrIdentityPredicate(x:Int=0) extends  Predicate[Int]{
    def apply(a:Int) = false
  }
  object IntTruePredicate extends TruePredicate[Int](0)
  object IntFalsePredicate extends FalsePredicate[Int](0)

  val l = List(1,2,3,4,5,6,7,8,9,10)

  def associativityOr = {
    val l1 = l.filter((IntLessThan(2) or IntGreaterThan(7)) or isOddPredicate())
    val l2 = l.filter(IntLessThan(2) or (IntGreaterThan(7) or isOddPredicate()))
    "Associativity Or" should "predicate composition meets Associativity Law" in {
      assert(l1 == l2)
    }
  }
  def associativityAnd= {
    val l1 = l.filter((IntLessThan(2) and IntGreaterThan(7)) and isOddPredicate())
    val l2 = l.filter(IntLessThan(2) and (IntGreaterThan(7) and isOddPredicate()))
    "Associativity And" should "predicate composition meets Associativity Law" in {
      assert(l1 == l2)
    }
  }
  def commutativityOr = {
    val l1 = l.filter(IntLessThan(2) or IntGreaterThan(7))
    val l2 = l.filter(IntGreaterThan(7) or IntLessThan(2))
    "Commutativity Or" should "predicate composition meets Commutativity Law" in {
      assert(l1 == l2)
    }
  }
  def commutativityAnd =  {
    val l1 = l.filter(IntLessThan(2) and IntGreaterThan(7))
    val l2 = l.filter(IntGreaterThan(7) and IntLessThan(2))
    "Commutativity And" should "predicate composition meets Commutativity Law" in {
      assert(l1 == l2)
    }
  }
  def distributivityAndOverOr = {
    val l1 = l.filter(isOddPredicate() and (IntLessThan(2) or IntGreaterThan(7)))
    val l2 = l.filter((isOddPredicate() and IntLessThan(2)) or (isOddPredicate() and IntGreaterThan(7)))
    "Distributivity And Over Or" should "predicate composition meets Distributivity Law" in {
      assert(l1 == l2)
    }
  }
  def IdentityForAnd = {
    val l1 = l.filter(isOddPredicate() and IntAndIdentityPredicate())
    val l2 = l.filter(isOddPredicate())
    "Identity For And" should "predicate composition meets Identity Law For And" in {
      assert(l1 == l2)
    }
  }
  def IdentityForOr = {
    val l1 = l.filter(isOddPredicate() or IntOrIdentityPredicate())
    val l2 = l.filter(isOddPredicate())
    "Identity For Or" should "predicate composition meets Identity Law For Or" in {
      assert(l1 == l2)
    }
  }
  def annihilatorForAnd = {
    val l1 = l.filter(isOddPredicate() and IntFalsePredicate)
    "Annihilator for And" should "predicate composition meets Annihilator Law" in {
      assert(l1 == List.empty[Int])
    }
  }
  def annihilatorForOr = {
    val l1 = l.filter(isOddPredicate() or IntTruePredicate)
    "Annihilator for Or" should "predicate composition meets Annihilator Law" in {
      assert(l1 == l)
    }
  }
  def idempotenceOfOr = {
    val l1 = l.filter(isOddPredicate() or isOddPredicate())
    val l2 = l.filter(isOddPredicate())
    "Idempotence of Or" should "predicate composition meets Idempotence Law" in {
      assert(l1 == l2)
    }
  }
  def idempotenceOfAnd = {
    val l1 = l.filter(isOddPredicate() and isOddPredicate())
    val l2 = l.filter(isOddPredicate())
    "Idempotence of And" should "predicate composition meets Idempotence Law" in {
      assert(l1 == l2)
    }
  }
  def absorption1 = {
    val l1 = l.filter(isOddPredicate() and (isOddPredicate() or IntGreaterThan(5)))
    val l2 = l.filter(isOddPredicate())
    "Absorption 1" should "predicate composition meets Absorption Law" in {
      assert(l1 == l2)
    }
  }
  def absorption2 = {
    val l1 = l.filter(isOddPredicate() or (isOddPredicate() and IntGreaterThan(5)))
    val l2 = l.filter(isOddPredicate())
    "Absorption 2" should "predicate composition meets Absorption Law" in {
      assert(l1 == l2)
    }
  }
  def distributivityOfOrOverAnd = {
    val l1 = l.filter(isOddPredicate() or (IntGreaterThan(3) and IntLessThan(7)))
    val l2 = l.filter((isOddPredicate() or IntGreaterThan(3) and (isOddPredicate() or IntLessThan(7))))
    "Distributivity Of Or Over And" should "predicate composition meets Distributivity Law" in {
      assert(l1 == l2)
    }
  }
  def complementation1 = {
    val l1 = l.filter(isOddPredicate() and !isOddPredicate())
    "Complementation 1" should "predicate composition meets Complementation Law" in {
      assert(l1 == List.empty[Int])
    }
  }
  def complementation2 = {
    val l1 = l.filter(isOddPredicate() or !isOddPredicate())
    "Complementation 2" should "predicate composition meets Complementation Law" in {
      assert(l1 == l)
    }
  }
  def doubleNegative = {
    val l1 = l.filter(!(!isOddPredicate()))
    val l2 = l.filter(isOddPredicate())
    "Double Negative" should "predicate composition meets Double Negative Law" in {
      assert(l1 == l2)
    }
  }
  def deMorgan1 = {
    val l1 = l.filter(!isOddPredicate() and !IntGreaterThan(3))
    val l2 = l.filter(!(isOddPredicate() or IntGreaterThan(3)))
    "De Morgan 1" should "predicate composition meets De Morgan Law" in {
      assert(l1 == l2)
    }
  }
  def deMorgan2 = {
    val l1 = l.filter(!isOddPredicate() or !IntGreaterThan(3))
    val l2 = l.filter(!(isOddPredicate() and IntGreaterThan(3)))
    "De Morgan 2" should "predicate composition meets De Morgan Law" in {
      assert(l1 == l2)
    }
  }

  associativityOr
  associativityAnd
  commutativityOr
  commutativityAnd
  distributivityAndOverOr
  IdentityForAnd
  annihilatorForAnd
  annihilatorForOr
  idempotenceOfOr
  idempotenceOfAnd
  absorption1
  absorption2
  distributivityOfOrOverAnd
  complementation1
  complementation2
  doubleNegative
  deMorgan1
  deMorgan2
}
