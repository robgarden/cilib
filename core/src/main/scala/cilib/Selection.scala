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
  import spire.math._
  import spire.implicits._

  val euclidean = Distance.euclidean[List,Double]

  def distanceNeighbours(n: Int) =
    (l: List[Particle[Mem[List,Double],List,Double]], x: Particle[Mem[List,Double],List,Double]) =>
      l.sortBy(li => euclidean(li.pos.pos, x.pos.pos)).take(n)

  def wheel[A]: Selection[A] =
    (l: List[A], a: A) => l match {
      case x :: _ if (x == l) => l
      case x :: xs => List(x, a)
    }

  val hamming = Distance.hamming[List,Boolean]

  def hypercube[A]: Selection[A] =
    (l: List[A], a: A) => {
      val length = (log(l.length.toDouble) / log(2)).toInt

      def pad(x: Seq[Boolean]) =
        if (x.length < length) (List.fill(length - x.length)(false) ++ x) else x

      def binary(a: Int) = pad(a.toBinaryString.map(x => if (x == '1') true else false)).toList

      val index = l.indexOf(a)
      val bin = binary(index)

      l.zipWithIndex.filter { case (xi, i) => hamming(binary(i), bin) == 1 }.map(_._1)
    }
}
