package cilib

import scalaz.{Foldable,Id,OneAnd,Maybe}
import scalaz.syntax.apply._
import scalaz.syntax.foldable1._
import scalaz.std.option._

object Sized {

  type Sized1[A] = Id.Id[A]
  type Sized2[A] = (A, A)
  type Sized3[A] = (A, A, A)
  type Sized4[A] = (A, A, A, A)
  type Sized5[A] = (A, A, A, A, A)
  type Sized6[A] = (A, A, A, A, A, A)
  type Sized10[A] = (A, A, A, A, A, A, A, A, A, A)

  type Sized1And[F[_], A] = OneAnd[F, A]
  final case class Sized2And[F[_], A](a: A, b: A, rest: F[A])
  final case class Sized3And[F[_], A](a: A, b: A, c: A, rest: F[A])

  def toSized1And[A](x: List[A]): Maybe[Sized1And[List,A]] =
    x match {
      case a :: rest => Maybe.just(OneAnd(a, rest))
      case _ => Maybe.empty
    }

  def toSized2And[A](x: List[A]): Maybe[Sized2And[List,A]] =
    x match {
      case a :: b :: rest => Maybe.just(Sized2And(a, b, rest))
      case _ => Maybe.empty
    }

  def toSized3And[A](x: List[A]): Maybe[Sized3And[List,A]] =
    x match {
      case a :: b :: c :: rest => Maybe.just(Sized3And(a, b, c, rest))
      case _ => Maybe.empty
    }

  def toSized1[F[_]: Foldable, A](x: F[A]): Option[Sized1[A]] = x.index(0)

  def toSized2[F[_]: Foldable, A](x: F[A]): Option[Sized2[A]] =
    (x.index(0) |@| x.index(1)) { (_, _) }

  def toSized3[F[_]: Foldable, A](x: F[A]): Option[Sized3[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2)) { (_, _, _) }

  def toSized4[F[_]: Foldable, A](x: F[A]): Option[Sized4[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3)) { (_, _, _, _) }

  def toSized5[F[_]: Foldable, A](x: F[A]): Option[Sized5[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3) |@| x.index(4)) { (_, _, _, _, _) }

  def toSized6[F[_]: Foldable, A](x: F[A]): Option[Sized6[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3) |@| x.index(4) |@| x.index(5)) {
      (_, _, _, _, _, _)
    }

}
