//> using scala "3"

def union[A](xs: Set[A]*): Set[A] = xs.foldLeft(Set[A]())(_ union _)

def union[A](xss: Set[Set[A]]): Set[A] = union(xss.flatten)

def intersection[A](xs: Set[A]*): Set[A] = xs.foldLeft(Set[A]())(_ intersect _)

def intersection[A](xss: Set[Set[A]]): Set[A] = intersection(xss.flatten)

def cardinality[A](s: Set[A]): Int = s.size

case class Rel[A, B](toSet: Set[(A, B)]):
  def image(a: A): Set[B] = toSet.collect { case (a2, b) if a2 == a => b }

  def image(as: Set[A]): Set[B] = toSet.collect { case (a, b) if as.contains(a) => b }

  lazy val domain: Set[A] = toSet.map((a,_) => a)

  lazy val range: Set[B] = toSet.map((_,b) => b)

  lazy val inverse: Rel[B, A] = Rel(toSet.map((a,b) => (b, a)))

  override def toString = toSet.toSeq.map((a,b) => s"$a -> $b").sorted.mkString("Rel(",", ", ")")

  export toSet.{toSet => _, concat => _, ++ => _, *}

object Rel:
  def apply[A, B](pairs: (A, B)*): Rel[A, B] = new Rel(pairs.toSet)
 
/** An endorelation is a relation with the same type of elements in the domain and range */
class EndoRel[A](override val toSet: Set[(A, A)]) extends Rel[A,A](toSet):
  override def toString = toSet.toSeq.map((a,b) => s"$a -> $b").sorted.mkString("EndoRel(",", ", ")")
  override def inverse: EndoRel[A] = new EndoRel(super.inverse.toSet)

  lazy val elems: Set[A] = domain ++ range

  lazy val elemPairs: Set[(A, A)] = (for a <- elems; b <- elems yield (a,b)).toSet

  lazy val elemTriples: Set[(A, A, A)] = (for a <- elems; b <- elems; c <- elems yield (a,b,c)).toSet

  lazy val isReflexive: Boolean = elems.forall(a => toSet.contains(a -> a))

  lazy val isSymmetric: Boolean = 
    elemPairs.forall((a, b) => 
      if toSet.contains(a -> b) then toSet.contains(b -> a) else true)
    // or: toSet.forall((a, b) => toSet.contains(b -> a))

  lazy val isAntiSymmetric: Boolean = ???

  lazy val isTransitive: Boolean = ???

object EndoRel:
  def apply[A](pairs: (A, A)*): EndoRel[A] = new EndoRel(pairs.toSet)

case class Poset[A](elems: Set[A], partialOrder: EndoRel[A]):
  def toEndoRel: EndoRel[A] = ???


extension [A, B](s: Iterable[(A, B)]) def toRel = Rel(s.toSet)

@main def run = println("hello")

