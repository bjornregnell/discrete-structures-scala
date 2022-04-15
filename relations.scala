//> using scala "3"

def union[A](xs: Set[A]*): Set[A] = xs.foldLeft(Set[A]())(_ union _)

def union[A](xss: Set[Set[A]]): Set[A] = union(xss.flatten)

def intersection[A](xs: Set[A]*): Set[A] = xs.foldLeft(Set[A]())(_ intersect _)

def intersection[A](xss: Set[Set[A]]): Set[A] = intersection(xss.flatten)

def cardinality[A](s: Set[A]): Int = s.size

case class Rel[A, B](toSet: Set[(A, B)]):
  def image(a: A): Set[B] = toSet.collect { case (a2, b) if a2 == a => b }

  def image(as: Set[A]): Set[B] = toSet.collect { case (a, b) if as.contains(a) => b }

  def domain: Set[A] = toSet.map((a,_) => a)

  def range: Set[B] = toSet.map((_,b) => b)

  def inverse: Rel[B, A] = Rel(toSet.map((a,b) => (b, a)))

  override def toString = toSet.toSeq.map((a,b) => s"$a -> $b").sorted.mkString("Rel(",", ", ")")

  export toSet.{toSet => _, concat => _, ++ => _, *}

object Rel:
  def apply[A, B](pairs: (A, B)*): Rel[A, B] = new Rel(pairs.toSet)
 
/** An endorelation is a relation with the same type of elements in the domain and range */
class EndoRel[A](override val toSet: Set[(A, A)]) extends Rel[A,A](toSet):
  override def toString = toSet.toSeq.map((a,b) => s"$a -> $b").sorted.mkString("EndoRel(",", ", ")")
  override def inverse: EndoRel[A] = new EndoRel(super.inverse.toSet)

  def elems: Set[A] = domain ++ range

  def elemPairs: Set[(A, A)] = (for a <- elems; b <- elems yield (a,b)).toSet

  def elemTriples: Set[(A, A, A)] = (for a <- elems; b <- elems; c <- elems yield (a,b,c)).toSet

  def isReflexive: Boolean = elems.forall(a => toSet.contains(a -> a))

  def isSymmetric: Boolean = 
    elemPairs.forall((a, b) => 
      if toSet.contains(a -> b) then toSet.contains(b -> a) else true)
      // same as: toSet.forall((a, b) => toSet.contains(b -> a))

  def isAntiSymmetric: Boolean = ???

  def isTransitive: Boolean = ???

object EndoRel:
  def apply[A](pairs: (A, A)*): EndoRel[A] = new EndoRel(pairs.toSet)

case class Poset[A](elems: Set[A], before: A => Option[A]):
  def toEndoRel: EndoRel[A] = ???


extension [A, B](s: Iterable[(A, B)]) def toRel = Rel(s.toSet)

@main def run = println("hello")

