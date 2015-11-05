package cilib

object Selection {

  import scalaz._
  import scalaz.syntax.std.option._

  implicit class RicherEphemeralStream[A](s: EphemeralStream[A]) {
    def drop(n: Int): EphemeralStream[A] = {
      def go(count: Int, c: Option[EphemeralStream[A]]): EphemeralStream[A] = {
        if (count > 0) {
          go(count - 1, c.flatMap(_.tailOption))
        } else c.cata(x => x, EphemeralStream())
      }

      go(n, Option(s))
    }
  }

  def indexNeighbours[A](n: Int): Selection[A] =
    (l: List[A], x: A) => {
      val size = l.size
      val point = (l.indexOf(x) - (n / 2) + size) % size
      lazy val c: EphemeralStream[A] = EphemeralStream(l: _*) ++ c

      c.drop(point).take(n).toList
    }

  import Scalaz._
  import spire.implicits._

  val euclidean = Distance.euclidean[List,Double]

  def distanceNeighbours(n: Int) =
    (l: List[Particle[Mem[List,Double],List,Double]], x: Particle[Mem[List,Double],List,Double]) =>
      l.sortBy(li => euclidean(li.pos.pos, x.pos.pos)).take(n)

  def wheel[A]: Selection[A] =
    (l: List[A], a: A) => l match {
      case x :: xs if (x == l) => l
      case x :: _ => List(x, a)
    }
}
