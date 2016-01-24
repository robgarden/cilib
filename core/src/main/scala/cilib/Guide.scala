package cilib

import _root_.scala.Predef.{any2stringadd => _, _}

import scalaz.Traverse
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

  def nbest[S,F[_],A](selection: Selection[Particle[S,F,A]])(implicit M: Memory[S,F,A]): Guide[S,F,A] = {
    (collection, x) => Step.withOpt(o => RVar.point {
      selection(collection, x).
        map(e => M._memory.get(e.state)).
        reduceLeft((a, c) => Fitness.compare(a, c) run o)
    })
  }

  def gbest[S,F[_],A](implicit M: Memory[S,F,A]): Guide[S,F,A] =
    nbest((c, _) => c)

  def lbest[S,F[_]](n: Int)(implicit M: Memory[S,F,Double]) =
    nbest(Selection.indexNeighbours[Particle[S,F,Double]](n))

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

  def undx[S](s1: Double, s2: Double, bounds: NonEmptyList[Interval[Double]])(implicit M: Memory[S,List,Double], MO: Module[List[Double],Double]): Guide[S,List,Double] =
    (collection, x) => {
      val gb = gbest
      val pb = pbest
      val undx = Crossover.undx(s1, s2, bounds)

      for {
        p         <- pb(collection, x)
        i         <- identity(collection, x)
        n         <- gb(collection, x)
        parents   =  NonEmptyList(p, i, n)
        offspring <- undx(parents)
      } yield offspring
    }

  def pcx[S,F[_]: Foldable : Zip](s1: Double, s2: Double)(implicit M: Memory[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {
      val gb = gbest
      val pb = pbest
      val pcx = Crossover.pcx[F](s1, s2)

      for {
        p         <- pb(collection, x)
        i         <- identity(collection, x)
        n         <- gb(collection, x)
        parents   =  NonEmptyList(p, i, n)
        offspring <- pcx(parents)
      } yield offspring
    }

  implicit def positionOrder[F[_]: Foldable]: Order[Position[F,Double]] = Order.orderBy(_.magnitude)

  def pcxPrev[S,F[_]: Foldable : Zip](s1: Double, s2: Double)(implicit M: Memory[S,F,Double], P: Previous[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {
      val gb = gbest
      val pb = pbest
      val prev = previous
      val pcx = Crossover.pcx[F](s1, s2)

      for {
        p         <- pb(collection, x)
        i         <- identity(collection, x)
        n         <- gb(collection, x)
        pv        <- prev(collection, x)
        parents   =  NonEmptyList(p, i, n)
        offspring <- if (parents.distinct.size == 3) pcx(parents) else pcx((pv <:: parents).distinct)
      } yield offspring
    }

  def pcxRepeater[S,F[_]:Foldable:Zip](s1: Double, s2: Double, retries: Int, parents: NonEmptyList[Position[F,Double]], selection: Selection[Entity[S,F,Double]])(implicit M: Memory[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {
      val guide = Guide.nbest[S,F,Double](selection)
      val nbest = guide(collection, x)
      val pcx = Crossover.pcx[F](s1, s2)
      val offspring = pcx(parents).flatMap(Step.evalF[F,Double])

      def repeat(r: Int): Step[F,Double,Position[F,Double]] =
        if (r >= retries) nbest
        else for {
          child    <- offspring
          nb       <- nbest
          isBetter <- Step.withOpt(o => RVar.point(Fitness.fittest(child, nb).run(o)))
          chosen   <- if (isBetter) offspring else repeat(r + 1)
        } yield chosen

      repeat(0)
    }

  def repeatingPCX[S,F[_]:Foldable:Zip](s1: Double, s2: Double, retries: Int, selection: Selection[Entity[S,F,Double]])(implicit M: Memory[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {

      val gb = gbest
      val pb = pbest

      for {
        p         <- pb(collection, x)
        i         <- identity(collection, x)
        n         <- gb(collection, x)
        parents   =  NonEmptyList(p, i, n)
        repeater  =  pcxRepeater(s1, s2, retries, parents, selection)
        chosen    <- repeater(collection, x)
      } yield chosen
    }

  def repeatingPCXNParents[S,F[_]:Foldable:Zip](s1: Double, s2: Double, retries: Int, n: Int, selection: Selection[Entity[S,F,Double]])(implicit M: Memory[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {

      val sample = RVar.sample(n, collection)
      val parents = sample.run.map(_.flatMap(l => l.toNel.toMaybe)).map(_.getOrElse(sys.error("Need 3 parents")))

      for {
        ps        <- Step.pointR(parents)
        pbparents =  ps.map(p => M._memory.get(p.state))
        repeater  =  pcxRepeater(s1, s2, retries, pbparents, selection)
        chosen    <- repeater(collection, x)
      } yield chosen

    }

  def pcxBolzmann[S,F[_]: Foldable : Zip](s1: Double, s2: Double, temp: Double, selection: Selection[Entity[S,F,Double]])(implicit M: Memory[S,F,Double], MO: Module[F[Double],Double]): Guide[S,F,Double] =
    (collection, x) => {
      val guide = Guide.nbest(selection)
      val pb = pbest
      val pcx = Crossover.pcx[F](s1, s2)

      def bolzmann(a: Position[F,Double], b: Position[F,Double]) = {
        val fitDiff = for {
          fa  <- a.fit
          fb  <- b.fit
          fav =  fa.fold(_.v, _.v)
          fbv =  fb.fold(_.v, _.v)
        } yield fav - fbv

        Dist.stdUniform.map(r => fitDiff.map { f =>
          val right = 1.0 / (1.0 + spire.math.exp(f / temp))
          if (r > temp) b else a
        })
      }

      for {
        p         <- pb(collection, x)
        i         <- identity(collection, x)
        n         <- guide(collection, x)
        parents   =  NonEmptyList(p, i, n)
        child     <- pcx(parents)
        offspring <- Step.evalF(child)
        chosen    <- Step.pointR(bolzmann(offspring, n))
      } yield chosen.getOrElse(n)
    }

  def nmpc[S,F[_]](implicit M: Module[F[Double], Double], F: Functor[F]): Guide[S,F,Double] =
    (collection, x) => {

      val col = collection.filter(_ != x)
      val chosen = RVar.sample(3, col).run
      val crossover = Crossover.nmpc[F]

      for {
        chos  <- Step.pointR(chosen)
        child <- chos.map(c => crossover(NonEmptyList(x.pos) :::> c.map(_.pos))).getOrElse(Step.point(x.pos))
      } yield child

    }

  def improved[S,F[_],A](t: Double)(implicit M: Memory[S,F,A]): Guide[S,F,A] =
    (collection, x) => {
      val gb = gbest[S,F,A]

      for {
        v <- Step.pointR(Dist.stdUniform)
        p <- Step.point(x.pos)
        y <- Step.point(M._memory.get(x.state))
        g <- gb(collection, x)
      } yield {
        if (v > 0 && v <= t) p
        else if (v > t && v <= ((1 + t) / 2)) y
        else g
      }
    }

  def improvedA[S,F[_]:Functor:Traverse:Zip,A](t: Double)(implicit M: Memory[S,F,A]): Guide[S,F,A] =
    (collection, x) => {
      val gb = gbest[S,F,A]

      for {
        v <- Step.pointR(x.pos.traverse(xi => Dist.stdUniform))
        p <- Step.point(x.pos)
        y <- Step.point(M._memory.get(x.state))
        g <- gb(collection, x)
        zipped = (((p zip y) zip g) zip v).map { case (((a, b), c), d) => (a, b, c, d) }
      } yield zipped.map { case (pi, yi, gi, vi) =>
        if (vi > 0 && vi <= t) pi
        else if (vi > t && vi <= ((1 + t) / 2)) yi
        else gi
      }
    }
}
