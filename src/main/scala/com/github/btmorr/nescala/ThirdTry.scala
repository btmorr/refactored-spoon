package com.github.btmorr.nescala

import cats.free.Free
import cats.free.Free.liftF
import cats.{Id, ~>}
import cats.data.State


object ThirdTry extends App {

  sealed trait ComputeResourceA[A]
  type ComputeResource[A] = Free[ComputeResourceA, A]
  // http://typelevel.org/cats/datatypes/freemonad.html
  /* A ComputeResourceA represents a computation that can be performed once
   * its inputs are available.
   *
   * Each case class represents a stage in the transformation
   * process, that ultimately yields a swarm of alchemical butterflies
   */
  final case class Coccoon[T](value: T) extends ComputeResourceA[T]
  def coccoon[T](value: T): ComputeResource[T] =
    liftF[ComputeResourceA, T](Coccoon[T](value))

  // Doesn't work (`wrong number of type parameters for method apply`). Probably need a different thing, but I don't know the name of it
//  final case class Differentiate[T](t: T) extends ComputeResourceA[T]
//  def differentiate[F[_], T](t: T, f: T => F[T]): ComputeResource[F[T]] =
//    liftF[ComputeResourceA, F[T]](Differentiate[F[_], T](f(t)))

  final case class Differentiate[T](t: T) extends ComputeResourceA[T]
  def differentiate[T](t: T, f: T => T): ComputeResource[T] =
    liftF[ComputeResourceA, T](Differentiate[T](f(t)))

  final case class Nectarize[Q, T](input: Q, f: Q => T) extends ComputeResourceA[T]
  def nectarize[Q, T](input: Q, f: Q => T): ComputeResource[T] =
    liftF[ComputeResourceA, T](Nectarize[Q, T](input, f))

  final case class Arborealize[Q, T](input: Q, f: Q => T) extends ComputeResourceA[T]
  def arborealize[Q, T](input: Q, f: Q => T): ComputeResource[T] =
    liftF[ComputeResourceA, T](Arborealize[Q, T](input, f))

  final case class Enplume[Q, T](input: Q, f: Q => T) extends ComputeResourceA[T]
  def enplume[Q, T](input: Q, f: Q => T): ComputeResource[T] =
    liftF[ComputeResourceA, T](Enplume[Q, T](input, f))

  case class TypedThing[T](value: T)
  val listOfFour: String => List[String] = s => List(s, s, s, s)
  val joinTogether: List[String] => String = ls => ls.mkString("*")
  val flurryOfActivity: String => String = s => joinTogether(listOfFour(s))
  val caseItUp: String => String = s => s.toUpperCase
  val wrapItUp: String => TypedThing[String] = s => TypedThing(s)
  val printIt: TypedThing[String] => Unit = in => println(s"$in")

  def program(s: String): ComputeResource[TypedThing[String]] =
    for {
      inString <- coccoon(s)
      // doesn't work, see above
      //i1 <- differentiate(inString, listOfFour)
      i1 <- differentiate(inString, flurryOfActivity)
      // any next step doesn't work if the prior thing produces a list, because it doesn't
      // iterate over the list (this would be the fork in the process--probably look to use
      // applicatives here?)
      i2 <- nectarize(i1, caseItUp)
      res <- enplume(i2, wrapItUp)
      _ <- arborealize(res, printIt)
    } yield res

  program("testing")

  type ComputationState[T] = State[Map[String, Any], T]
  val impureCompiler: ComputeResourceA ~> Id =
    new (ComputeResourceA ~> Id) {
      def apply[A](fa: ComputeResourceA[A]): Id[A] =
        fa match {
          case Coccoon(v) => v
          // this illustrates one of the weirdnesses. f happens prior to the creation
          // of the Differentiate. Do the classes need to represent the results of
          // the computation? If so, is this block only for the side-effects that are
          // meant to happen after?
          case Differentiate(v) => v
          // given the look at Differentiate above and when the f is applied, what
          // about these three, where it gets applied in here? do they both work?
          case Nectarize(v, f) => f(v)
          case Arborealize(v, f) => f(v)
          case Enplume(v, f) => f(v)
        }
    }

  val result: TypedThing[String] =
    program("hi mom").foldMap(impureCompiler)
  println(s"Second try result: $result")


  /* Specific questions:
   * - how do we include a step that might take one thing and give a collection of things,
   *       where a successive stage might want to map over the resulting collection?
   * - how to use Applicatives to accomplish the parallelism?
   * - what _really_ is gained by doing this over just writing pure functions that do the things
   *       inside of the compiler and/or program? It seems like a lot of boilerplate...
   * - how do you do the introspection mid-stream and use this for testing and whatnot? [people
   *       say that you can do this as if it's obvious, which makes me feel dumb]
   */
}
