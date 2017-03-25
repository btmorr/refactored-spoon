package com.github.btmorr.nescala

//import scalaz.Functor
//import scalaz.syntax.functor._
import cats.free.Free
import cats.free.Free.liftF
import cats.arrow.FunctionK
import cats.{Id, ~>}

import scala.collection.mutable

object FirstTry extends App {

  sealed trait ComputeResourceA[A]
  type ComputeResource[A] = Free[ComputeResourceA, A]
  // Essentially copy-pasta from http://typelevel.org/cats/datatypes/freemonad.html
  /* A ComputeResourceA represents a computation that can be performed once
   * its inputs are available.
   *
   * A choose is just a Run(options: List[A]): ComputeResourceA[A]
   * Defer represents "I'm not going to do this now, but I might when I resume
   * Skip represents "I'm not going to do this, now or in the future--move
   * on (this requires downstream nodes to either have this output as an
   * optional input, or to have an alternative source of input).
   */
  final case class Hold[T](value: T) extends ComputeResourceA[T]
  def hold[T](value: T): ComputeResource[T] =
    liftF[ComputeResourceA, T](Hold[T](value))
  final case class Run[Q, T](input: Q, f: Q => T) extends ComputeResourceA[T]
  def run[Q, T](input: Q, f: Q => T): ComputeResource[T] =
    liftF[ComputeResourceA, T](Run[Q, T](input, f))
  case object Defer extends ComputeResourceA[Unit]
  def defer =
    liftF[ComputeResourceA, Unit](Defer)
  case object Skip extends ComputeResourceA[Unit]
  def skip =
    liftF[ComputeResourceA, Unit](Skip)

  val upCase: String => String = s => s.toUpperCase
  val double: String => String = s => s+s
  val printIt: String => Unit = s => println(s)
  def program(s: String): ComputeResource[String] =
    for {
      inString <- hold(s)
      interm <- run(inString, upCase)
      res <- run(interm, double)
      _ <- run(res, printIt)
    } yield res

  program("testing")
  // still not _quite_ 100% sure why it doesn't run yet, but ok

  sealed trait StoreA[A]
  type Store[A] = Free[StoreA, A]
  /* Represents operations on an underlying kv store, as in the cats docs
   */
  final case class Put[T](key: String, value: T) extends StoreA[Unit]
  def put[T](key: String, value: T): Store[Unit] =
    liftF[StoreA, Unit](Put[T](key, value))
  final case class Get[T](key: String) extends StoreA[Option[T]]
  def get[T](key: String): Store[Option[T]] =
    liftF[StoreA, Option[T]](Get[T](key))
  final case class Delete(key: String) extends StoreA[Unit]
  def delete(key: String): Store[Unit] =
    liftF(Delete(key))

  def update[T](key: String, f: T => T): Store[Unit] =
    for {
      vMaybe <- get[T](key)
      _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
    } yield ()

  def program2: Store[Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  // the program will crash if a key is not found,
  // or if a type is incorrectly specified.
  def impureCompiler: StoreA ~> Id  =
    new (StoreA ~> Id) {

      // a very simple (and imprecise) key-value store
      val kvs = mutable.Map.empty[String, Any]

      def apply[A](fa: StoreA[A]): Id[A] =
        fa match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs(key) = value
            ()
          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).map(_.asInstanceOf[A])
          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }
    }

  val result: Option[Int] = program2.foldMap(impureCompiler)
  println(s"Cats example result: $result")

  import cats.data.State

  type KVStoreState[A] = State[Map[String, Any], A]
  val pureCompiler: StoreA ~> KVStoreState = new (StoreA ~> KVStoreState) {
    def apply[A](fa: StoreA[A]): KVStoreState[A] =
      fa match {
        case Put(key, value) => State.modify(_.updated(key, value))
        case Get(key) =>
          State.inspect(_.get(key).map(_.asInstanceOf[A]))
        case Delete(key) => State.modify(_ - key)
      }
  }

  val result2: (Map[String, Any], Option[Int]) = program2.foldMap(pureCompiler).run(Map.empty).value
  println(s"Result2: $result2")
//  // See https://tpolecat.github.io/2014/03/21/functor.html
//  case class Content[A](d: A) {
//    println(s"Created a content with value $d")
//  }
//
//  implicit val contentFunctor = new Functor[Content] {
//    def map[A, B](fa: Content[A])(f: A => B): Content[B] = Content(f(fa.d))
//  }
//
//  val ContentFunctor = Functor[Content]
//
//  val c1 = Content[Int](1)
//  val doubled = c1.map(_ * 2)
//  println(s"Doubled: $doubled")
//
//  sealed trait Computation[A]
//
//  object Computation {
//    final case class Run(input: String) extends Computation[Content[String]]
//  }


}
