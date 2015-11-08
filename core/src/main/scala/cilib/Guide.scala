package cilib

object Guide {

  def identity[S,F[_],A]: Guide[S,F,A] =
    (_, x) => Step.point(x.pos)

  def pbest[S,F[_],A](implicit M: Memory[S,F,A]): Guide[S,F,A] =
    (_, x) => Step.point(M._memory.get(x.state))

  def nbest[S,F[_]](selection: Selection[Particle[S,F,Double]])(implicit M: Memory[S,F,Double]): Guide[S,F,Double] = {
    (collection, x) => Step.withOpt(o => RVar.point {
      selection(collection, x).
        map(e => M._memory.get(e.state)).
        reduceLeft((a, c) => Fitness.compare(a, c) run o)
    })
  }

  def gbest[S,F[_]](implicit M: Memory[S,F,Double]): Guide[S,F,Double] =
    nbest((c, _) => c)

  def lbest[S,F[_]](n: Int)(implicit M: Memory[S,F,Double]) =
    nbest(Selection.indexNeighbours[Particle[S,F,Double]](n))

  import scalaz._
  import Scalaz._
  import spire.implicits._

  def fer[S,F[_]: Foldable](s: Double)(implicit M: Memory[S,F,Double]): Guide[S,F,Double] = {
    (collection, x) => Step.withOpt(o => RVar.point {
      val sorted = collection.map(e => M._memory.get(e.state)).sortWith((a, c) => Fitness.fittest(a,c) run o)

      val scale = for {
        b <- sorted.head.fit
        w <- sorted.last.fit
      } yield s / (b.fold(_.v,_.v) - w.fold(_.v,_.v))

      val euclid = Distance.euclidean[F,Double]

      def ratio(a: Particle[S,F,Double], b: Particle[S,F,Double]) = {
        val denom = euclid(M._memory.get(a.state).pos, M._memory.get(b.state).pos)
        val numer = for {
          fpa <- M._memory.get(a.state).fit
          fpb <- M._memory.get(b.state).fit
        } yield fpa.fold(_.v,_.v) - fpb.fold(_.v,_.v)

        for {
          n <- numer
          s <- scale
        } yield s * (n / denom)
      }

      def choose(a: Particle[S,F,Double], b: Particle[S,F,Double]) = {
        val chosen = for {
          r1 <- ratio(a, x)
          r2 <- ratio(b, x)
        } yield if (r1 > r2) a else b
        chosen.getOrElse(x)
      }

       collection.filter(_ != x).reduceLeft((a, b) => choose(a, b)).pos
    })
  }

}
