package cilib

import _root_.scala.Predef.{any2stringadd => _}

// import scalaz._
// import Scalaz._
import spire.math.{abs,pow}
import spire.algebra.Module
import cilib.Position._

object Guide {

  def identity[S,F[_],A]: Guide[S,F,A] =
    (_, x) => Step.point(x.pos)

  def previous[S,F[_],A](implicit P: Previous[S,F,A]): Guide[S,F,A] =
    (_, x) => Step.point(P._previous.get(x.state))

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

  import scalaz.Traverse

  def fips[S,F[_]: Traverse](selection: Selection[Particle[S,F,Double]], c1: Double, c2: Double)
    (implicit M: Memory[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =

    (collection, x) => {
      val neighbours = selection(collection, x)
      val avgGuide = neighbours.map(xj => M._memory.get(xj.state) - x.pos).reduce(_ + _)

      val guide = avgGuide.traverse { ai =>
        Dist.uniform(0.0, c1 + c2).map { rt =>
          (rt * ai) / neighbours.length
        }
      }

      Step.pointR(guide)
    }

    import scalaz._, Scalaz._
    import spire.implicits._

  def fer[S,F[_]: Foldable](s: Double)(implicit M: Memory[S,F,Double]): Guide[S,F,Double] =
    (collection, x) => Step.withOpt(o => RVar.point {
      val sorted = collection.map(e => M._memory.get(e.state)).sortWith((a, c) => Fitness.fittest(a, c) run o)

      val scale = for {
        b <- sorted.head.fit
        w <- sorted.last.fit
        bf = b.fold(_.v,_.v)
        bw = w.fold(_.v,_.v)
        denom = o match {
          case Min => bw - bf
          case Max => bf - bw
        }
      } yield s / denom

      val euclid = Distance.euclidean[F,Double]

      def ratio(a: Particle[S,F,Double], b: Particle[S,F,Double]) = {
        val denom = euclid(M._memory.get(a.state).pos, M._memory.get(b.state).pos)
        val numerator = for {
          fpa <- M._memory.get(a.state).fit
          fpb <- M._memory.get(b.state).fit
          numer = fpa.fold(_.v,_.v) - fpb.fold(_.v,_.v)
          numer1 = o match {
            case Min => -numer
            case Max => numer
          }
        } yield numer1

        for {
          numer <- numerator
          s <- scale
        } yield s * (numer / denom)
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
          numer1 = o match {
            case Min => -numer
            case Max => numer
          }
          denom = abs(pjd - xd)
        } yield (numer1 / denom, pjd)
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

  def pcxBase[F[_]: Foldable : Zip](parents: NonEmptyList[Position[F,Double]], s1: Double, s2: Double)(implicit M: Module[F[Double],Double]): Position[F,Double] = {
    val mean = Position.mean(parents)
    val k = parents.size

    val initEta = NonEmptyList(parents.last - mean)
    val (dd, e_eta) = parents.foldLeft((0.0, initEta)) { (a, b) =>
      val d = b - mean

      if (d.isZero) a
      else {
        val e = d.orthogonalize(a._2)

        if (e.isZero) a
        else (a._1 + e.magnitude, a._2 append NonEmptyList(e.normalize))
      }
    }

    val child = parents.last + (s1 *: e_eta.head)

    val distance = if (k > 2) dd / (k - 1) else 0.0
    e_eta.tail.foldLeft(child) { (c, e) => c + (s2 *: (distance *: e)) }
  }

  def pcx[S,F[_]: Foldable : Zip](s1: Double, s2: Double)(implicit M: Memory[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {
      val gb = gbest
      val pb = pbest

      for {
        p         <- pb(collection, x)
        i         <- identity(collection, x)
        n         <- gb(collection, x)
        parents   =  NonEmptyList(p, i, n)
        sigma1    <- Step.pointR(Dist.gaussian(0.0, s1))
        sigma2    <- Step.pointR(Dist.gaussian(0.0, s2))
      } yield pcxBase(parents, sigma1, sigma2)
    }

  implicit def positionOrder[F[_]: Foldable]: Order[Position[F,Double]] = Order.orderBy(_.magnitude)

  def pcxPrev[S,F[_]: Foldable : Zip](s1: Double, s2: Double)(implicit M: Memory[S,F,Double], P: Previous[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {
      val gb = gbest
      val pb = pbest
      val prev = previous

      for {
        p         <- pb(collection, x)
        i         <- identity(collection, x)
        n         <- gb(collection, x)
        pv        <- prev(collection, x)
        parents   =  NonEmptyList(p, i, n)
        sigma1    <- Step.pointR(Dist.gaussian(0.0, s1))
        sigma2    <- Step.pointR(Dist.gaussian(0.0, s2))
      } yield {
        if (parents.distinct.size == 3) pcxBase(parents, sigma1, sigma2)
        else pcxBase((pv <:: parents).distinct, sigma1, sigma2)
      }
    }
}
