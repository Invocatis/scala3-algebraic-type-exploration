package example

package galactus2

import scala.util.NotGiven
import cats.Distributive
import scala.annotation.implicitNotFound

object TypeStuff {

  type ∃[T] = NotGiven[NotGiven[T]]

  def instance[T]: T = null.asInstanceOf[T]

  // ----------------------------------
  // Properties

  trait Reflexive[∘[_, _]] {

    given reflexive[A]: (A ∘ A) = instance[(A ∘ A)]

  }

  trait Transitive[∘[_, _]] {

    given transitive[A, B, C](using A ∘ B, B ∘ C): (A ∘ C) = instance

  }

  trait Symmetric[∘[_, _]] {
    given symmetric[A, B](using A ∘ B): (B ∘ A) = instance
  }


  trait Commutative[∘[_, _]] {
    given commutative[A, B]: ((A ∘ B) =:= (B ∘ A)) = instance
  }


  trait Associative[∘[_, _]] {

    given eqAssociative[A, B, C]: ((A ∘ (B ∘ C)) =:= ((A ∘ B) ∘ C)) = instance
    given lrAssociative[A, B, C](using (A ∘ B) ∘ C): (A ∘ (B ∘ C)) = instance
    given rlAssociative[A, B, C](using A ∘ (B ∘ C)): ((A ∘ B) ∘ C) = instance

  }

  trait Distributive[⊕[_, _], ∘[_, _]] {
    given distributive[A, B, C](using Distributive[⊕, ∘]): ((A ∘ (B ⊕ C)) =:= ((A ∘ B) ⊕ (A ∘ C))) = instance
  }


  // ----------------------------------

  type ¬[A] = NotGiven[A]

  sealed trait TypeSet

  object EmptyTypeSet extends TypeSet

  val ∅ : EmptyTypeSet.type = EmptyTypeSet

  type ∅ = EmptyTypeSet.type

  type &&[A, B] = TypeSetPair[A, B]
  case class TypeSetPair[A, B](left: A, right: B) extends TypeSet {
    override def toString: String = s"$left && $right"
  }

  object TypeSetPair extends Commutative[&&] with Associative[&&] {

    given identity[A, B](using B ∈ A): ((A && B) =:= A) with {}

  }

  trait Set[A, B] {
    def apply(a: A)(b: B): A && B
  }

  object Set {

    given [A, B](using B ∈ A): Set[A, B] with {
      def apply(a: A)(b: B): A && B = ???
    }

  }

  extension [A](a: A) {
    def &&[B](b: B): (A && B) = TypeSetPair(a, b)
  }

  sealed trait Get[From, T] {
    def value(from: From): T
  }

  object Get {

    given [A]: Get[A, A] with {
      def value(a: A): A = a
    }

    given [A, B]: Get[A && B, A] with {
      def value(a: A && B): A = a.left
    }

    given [A, B, C](using Get[A, C]): Get[A && B, C] with {
      def value(a: A && B): C = summon[Get[A, C]].value(a.left)
    }


    extension [From](from: From) {
      def get[T](using Get: Get[From, T]): T = Get.value(from)
    }

  }

  type =:=[A, B] = Equal[A, B]
  @implicitNotFound(msg = "Cannot prove that ${A} =:= ${B}.")
  trait Equal[A, B]
  object Equal extends Reflexive[=:=] with Transitive[=:=] with Symmetric[=:=]

  type =!=[A, B] = NotEqual[A, B]
  trait NotEqual[A, B]
  object NotEqual {

    given [A, B](using NotGiven[A =:= B]): (A =!= B) with {}

  }

  type ∈[A, B] = Contains[A, B]
  trait Contains[A, B]
  object Contains {

    given [S]: Contains[S, S] with {}

    given [A, B]: Contains[A, A && B] with {}

  }

  type ⊆[A, B] = Subset[A, B]
  @implicitNotFound(msg = "Cannot prove that ${A} ⊂ ${B}.")
  trait Subset[A, B]
  object Subset extends Reflexive[⊆] with Transitive[⊆] with Symmetric[⊆] {

    given a[A]: Subset[Nothing, A] with {}

    given b[A, B]: (A ⊆ (A && B)) with {}

    given c[A, B]: (A ⊆ (B && A)) with {}

  }

  type ∩[A, B] = Intersection[A, B]

  trait Intersection[A, B]

  object Intersection extends Commutative[∩] {

    given Associative[∩] with {}

    given a[A]: ((A ∩ Nothing) =:= Nothing) with {}

    given b[A]: ((A ∩ A) =:= A) with {}

    given c[A, B, C, D](using (A ∩ B) =:= D): (((A && C) ∩ (B && C)) =:= (D && C)) with {}

    given d[A, B](using NotGiven[A ∩ B]): ((A ∩ B) =:= Nothing) with {}
  }



  type ?∩[A, B] = Intersecting[A, B]
  trait Intersecting[A, B]

  object Intersecting {

    given [A]: Intersecting[A, A] with {}
    given x[A, B, C]: Intersecting[A && C, A && B] with {}

  }


  type \[A, B] = Difference[A, B]
  trait Difference[A, B]
  object Difference {

    given a[A]: ((A \ Nothing) =:= A) with {}

    given b[A]: ((A \ A) =:= Nothing) with {}

    given c[A, B]: (((A && B) \ B) =:= A) with {}

    given d[A, B, C](using A \ B): (((A && C) \ (B && C)) =:= (A \ B)) with {}

    given e[A, B](using NotGiven[A \ B]): ((A \ B) =:= A) with {}

  }


  val x: Int = 1
}
