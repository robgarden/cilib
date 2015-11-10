package cilib

import scalaz._
import Scalaz._
import spire.math.abs
import spire.implicits._


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

  def fer[S,F[_]: Foldable](s: Double)(implicit M: Memory[S,F,Double]): Guide[S,F,Double] =
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

       collection.filter(_ != x).reduce(choose).pos
    })

  def fdr[S](implicit M: Memory[S,List,Double]): Guide[S,List,Double] =
    (collection, x) => Step.withOpt(o => RVar.point {

      def ratioWithValue(j: Particle[S,List,Double], d: Int) = {
        val pj = M._memory.get(j.state)
        val zipped = pj.pos.zip(x.pos.pos)
        val dimension = zipped.zipWithIndex
          .find { case ((_, _), i) => i == d }
          .map  { case ((a, b), _) => (a, b) }
          .toMaybe

        for {
          fpj <- pj.fit
          fx  <- x.pos.fit
          (pjd, xd) <- dimension
          numer = fpj.fold(_.v,_.v) - fx.fold(_.v,_.v)
          denom = abs(pjd - xd)
        } yield (numer / denom, pjd)
      }

      def bestRatio(a: (Double,Double), b: (Double,Double)) =
        if (a._1 > b._1) a else b

      val col = collection.filter(_ != x)
      val zipped = x.pos.pos.toList.zipWithIndex

      val pn = for {
        (_, d) <- zipped
        best = col.traverse(j => ratioWithValue(j, d)).map(_.reduce(bestRatio))
      } yield best

      val pos = pn.sequence.map(_.map(_._2))

      pos.map(Position(_)).getOrElse(x.pos)
    })

}
