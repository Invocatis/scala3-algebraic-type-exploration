package example

import scala.util.NotGiven

import TypeStuff._
import _root_.galactus2.TypeStuff.Difference.a

object TypeStuffTest {


  def ∃[T](using NotGiven[NotGiven[T]]): T = ???
  def ∄[T](using NotGiven[T]): Unit = ???

  def =?=[A, B](using NotGiven[NotGiven[A =:= B]]): Unit = ???
  def =!=[A, B](using NotGiven[A =:= B]): Unit = ???

  type A
  type B
  type C
  type D

  object Reflexive {

    type ∘[A, B] = Op[A, B]
    trait Op[A, B]
    object Op extends TypeStuff.Reflexive[∘]

    ∃[A ∘ A]
    ∄[A ∘ B]

  }

  object Transitive {

    type ∘[A, B] = Op[A, B]
    trait Op[A, B]
    object Op extends TypeStuff.Transitive[∘]


    given (A ∘ B) with {}
    given (B ∘ C) with {}

    ∃[A ∘ C]
  }

  object Associative {

    type ∘[A, B] = Op[A, B]
    trait Op[A, B]
    object Op extends TypeStuff.Associative[∘]

    given x: (A ∘ (B ∘ C)) with {}

    ∃[(A ∘ B) ∘ C]

    =?=[A ∘ (B ∘ C), (A ∘ B) ∘ C]
    =?=[(A ∘ B) ∘ C, A ∘ (B ∘ C)]

  }

  object TypeSet {

    =?=[A && Nothing, A]
    =!=[A && B, A]
    =?=[A && B, A && B]
    =?=[A && B, B && A]
    =?=[(A && B) && C, A && (B && C)]

    =?=[(A && B) && C, C && (A && B)]

    // =?=[(A && B) && C, (C && A) && B](using Associative.evidence)

  }

  object Get {

    import TypeStuff.Get._

    (1).get[Int]
    (1 && ∅).get[Int]
    (1 && "asdf" && ∅).get[Int]
    (1 && "asdf" && 2).get[Int]

  }

  object Contains {
    ∃[A ∈ (A && B)]
    ∃[A ∈ A]
    ∄[A ∈ Nothing]
  }

  object Subset {

    ∄[A ⊆ Nothing]
    ∄[A ⊆ B]
    ∄[A ⊆ (B && C)]
    ∃[A ⊆ A]
    ∃[A ⊆ (A && B)]
    ∃[(A && B) ⊆ (A && B)]
    ∃[(A && B) ⊆ (A && B && C)]

    ∃[C ⊆ ((B && C) && (A && (B - B)))]
    ∃[C ⊆ ((B && C) && (A && Nothing))]
    ∃[C ⊆ ((B && C) && A)]
    ∃[C ⊆ (B && C)]
    ∃[C ⊆ (C && B)]

  }

  object Intersection {

    =?=[A ∩ B, Nothing]
    =?=[A ∩ B, B ∩ A]
    =?=[A ∩ A, A]
    val x: (((A && C) ∩ (B && C)) =:= (Nothing && C)) = TypeStuff.Intersection.c[A, B, C, Nothing]
    =?=[(A && C) ∩ (B && C), C]
    =?=[(A && B) ∩ A, A ∩ (A && B)]
    =?=[(A && B) ∩ A, A ∩ (B && A)]
    =!=[(A && B) ∩ A, B ∩ (B && A)]

  }

  object Difference {

    =?=[A \ Nothing, A]
    =?=[A \ A, Nothing]

    =?=[(A && B) \ B, A]
    // =?=[(A && C) \ (B && C), A](using TypeStuff.Difference.d[A, B, C](using TypeStuff.Difference.e[A, B]))

    =?=[A \ B, A]

  }

}


object X {

  def test[T](using NotGiven[NotGiven[T]]): Unit = ???

  trait X
  given a: X with {}
  given b: X with {}


  test[X]
}