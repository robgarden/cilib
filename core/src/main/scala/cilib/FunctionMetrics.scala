package cilib

import scalaz._
import Scalaz._
import scalaz.Ordering._

import spire.algebra._
import spire.math._
import spire.implicits._

import benchmarks.Benchmarks.{Sized2And, Sized3And, toSized2And, toSized3And}

object FunctionMetrics {

  type FunctionMetric = List[Position[List, Double]] => Maybe[Double]

  def distance(a: Position[List, Double], b: Position[List, Double]) =
    sqrt((a - b).pos.map(_ ** 2).sum)

  def distanceVector(a: List[Double], b: List[Double]) =
    sqrt((a zip b).map { case (ai, bi) => (ai - bi) ** 2 }.sum)

  def fdc(opt: Opt): FunctionMetric =
    solutions => {
      val fitnesses: Maybe[List[Fit]] = solutions.traverse(_.fit)

      val F = for {
        fit <- fitnesses
        v = fit.map {
          case Valid(v) => v
          case Penalty(v, _) => v
        }
      } yield v

      val fittest = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run opt)

      for {
        f      <- F
        fbar   = f.sum / f.length
        dstar  = solutions.map(s => distance(s, fittest))
        dbar   = dstar.sum / dstar.length
        zipped = f zip dstar
        numer  = zipped.map { case (fi, di) => (fi - fbar) * (di - dbar) }.sum
        denom1 = sqrt(f.map(fi => (fi - fbar) ** 2).sum)
        denom2 = sqrt(dstar.map(di => (di - dbar) ** 2).sum)
      } yield numer / (denom1 * denom2)
    }

  val fem: Sized3And[List, Position[List, Double]] => Maybe[Double] =
    solutions => {

      def S(e: Double, x: Sized2And[List, Double]): List[Int] = {
        def calcS(a: Double, b: Double): Int =
          if (b - a < -e) -1
          else if (math.abs(b - a) <= e) 0
          else 1

          val points = x.a :: x.b :: x.rest.toList
          points.sliding(2).toList.map {
            case Seq(a, b) => calcS(a, b)
          }
      }

      def isLandscapeFlat(x: List[Int]) = x.forall(_ == 0)

      def infoStability(x: Sized2And[List, Double]): Double = {
        // eb, es, e, eo
        type IS = (Double, Double, Double, Int)

        def fbo(is: IS): IS = is match {
          case (eb, es, e, eo) =>
            val s = S(e, x)
            if (isLandscapeFlat(s)) (eb, es / eb, e, eo)
            else fbo((eb, es * eb, es, eo + 1))
        }

        val is: IS = fbo((10, 10, 0, 0))

        val smallestStep = 0.01 * math.pow(10, is._4.toDouble)

        def nfp(is: IS): IS =
          is match {
            case (eb, es, e, eo) =>
              val s = S(e, x)
              val flat = isLandscapeFlat(s)
              if (flat) {
                if (es <= smallestStep) {
                  is
                } else {
                  val e1 = e - es
                  val es1 = es / eb
                  val e2 = e1 + es1
                  nfp((eb, es1, e2, eo))
                }
              } else nfp((eb, es, e + es, eo))
          }

        val is1 = nfp(is)
        val e = is1._3
        e
      }

      def infoContent(e: Double, x: Sized2And[List, Double]) = {
        val s = S(e, x)

        def hash(p: Int, q: Int): Int = {
          val x = -p + 2 * q
          if (x < 0) x + 3
          else x + 2
        }

        val grouped = s.sliding(2).toList
          .filter  { case Seq(p, q) => p != q }
          .map     { case Seq(p, q) => hash(p, q) }
          .groupBy { x => x }

          val entropy = grouped.values.toList.map(_.length).map {
            ent =>
              val prob = ent.toDouble / (s.length - 1)
              prob * (log(prob) / log(6))
          }

          -entropy.sum
      }

      val points = solutions.a :: solutions.b :: solutions.c :: solutions.rest.toList
      val fitnesses: Maybe[List[Fit]] = points.traverse(_.fit)

      val f: Maybe[List[Double]] = fitnesses.map(_.map(_.fold(_.v, _.v)))

      val increment = 0.05
      val numEpsilons = (1.0 / increment).toInt + 1

      def getEpsilons(e: List[Double], mult: Double, i: Int, es: Double): List[Double] =
        if (i < numEpsilons) getEpsilons(e.updated(i, es * mult), mult + increment, i + 1, es)
        else e

        for {
          fi          <- f
          x2          <- toSized2And(fi)
          epsilonStar =  infoStability(x2)
          epsilons    =  getEpsilons(List.fill(numEpsilons)(0), 0.0, 0, epsilonStar)
          es          =  epsilons.map(e => infoContent(e, x2))
        } yield es.max
    }

  def dispersion(threshold: Double, opt: Opt, domain: NonEmptyList[Interval[Double]]): FunctionMetric =
    solutions => {
      val dimension = domain.size
      val fullDispersion = Math.sqrt(3.0 * dimension) / 4.0 - 0.1

      val amount = (solutions.length * threshold).toInt
      val bestPoints = solutions.sortWith((a,b) => opt.order(a.fit, b.fit) == GT).take(amount)

      def normalise(position: Position[List,Double]) = {
        val normPos = (position.pos zip domain.list).map {
          case (xi, bound) =>
            val upper = bound.upper.value
            val lower = bound.lower.value
            (xi - lower) / (upper - lower)
        }
        Position(normPos)
      }

      val normalised = bestPoints.map(normalise)

      val distances = normalised.sliding(2).toList.map {
        case Seq(a, b) => distance(a, b)
      }

      val avg = distances.sum / distances.length

      (avg - fullDispersion).just
    }

  def informationLandscape(opt: Opt): FunctionMetric =
    solutions => {

      val fitnesses: Maybe[List[Fit]] = solutions.traverse(_.fit)
      val F: Maybe[List[Double]] = fitnesses.map(_.map(_.fold(_.v, _.v)))
      val F1: Maybe[Sized2And[List, Double]] = F.flatMap(toSized2And)

      def il(a: Double, b: Double) =
        if (a < b) 1.0
        else if (a == b) 0.5
        else 0

      def ilVector(x: Sized2And[List, Double]): List[Double] =
        (x.a :: x.b :: x.rest.toList)
          .sliding(2).toList.map { case Seq(f1, f2) => il(f1, f2) }

      val best = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run opt)
      val pointsIlVector = F1.map(ilVector)

      def sphere(x: List[Position[List,Double]]): List[Double] =
        for {
          p <- x
          f = (p.pos zip best.pos)
            .map    { case (pi, bi) => (pi - bi) ** 2 }
            .reduce { (x1, x2) => x1 + x2 }
        } yield f

      val sphereSolutions = sphere(solutions)
      val sphereSolutionsIlVector = toSized2And(sphereSolutions).map(ilVector)

      for {
        pv <- pointsIlVector
        sv <- sphereSolutionsIlVector
      } yield distanceVector(pv, sv)
    }

  def gradients(stepSize: Double, domain: NonEmptyList[Interval[Double]]): List[Position[List,Double]] => Maybe[List[Double]] =
    (solutions) => {
      val best = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run Min)
      val worst = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run Max)

      val fMax = worst.fit.map(_.fold(_.v, _.v))
      val fMin = best.fit.map(_.fold(_.v, _.v))
      val fitRange = for {
        max <- fMax
        min <- fMin
      } yield max - min

      val domainRange = domain.list.map(i => i.upper.value - i.lower.value).sum

      solutions.sliding(2).toList.traverse {
        case Seq(s1, s2) => for {
          f1 <- s1.fit
          f2 <- s2.fit
          fr <- fitRange
          f1v = f1.fold(_.v, _.v)
          f2v = f2.fold(_.v, _.v)
          deltaX = ((f2v - f1v) / fr)
          deltaY = stepSize / domainRange
        } yield deltaX / deltaY
      }
    }

  def gradientAvg(stepSize: Double, domain: NonEmptyList[Interval[Double]]): FunctionMetric =
    (solutions) =>
      for {
        g <- gradients(stepSize, domain)(solutions)
        sum = g.map(abs).sum
      } yield sum / solutions.length

  def gradientDev(stepSize: Double, domain: NonEmptyList[Interval[Double]]): FunctionMetric =
    (solutions) =>
      for {
        a <- gradientAvg(stepSize, domain)(solutions)
        g <- gradients(stepSize, domain)(solutions)
        dev = g.map(gt => (a - abs(gt)) ** 2).sum
      } yield sqrt(dev / (solutions.length - 1))

  def gradientMax(stepSize: Double, domain: NonEmptyList[Interval[Double]]): FunctionMetric =
    (solutions) =>
      for {
        g <- gradients(stepSize, domain)(solutions)
      } yield g.map(abs).max

  def fciCog(opt: Opt, domain: NonEmptyList[Interval[Double]], n: Int): Eval[List,Double] => RVar[Maybe[Double]] =
    eval => {
      import cilib.Defaults.cognitive

      val pbest = Guide.pbest[Mem[List,Double],List,Double]

      val pso = cognitive(0.729844, 1.496180, pbest)

      val points0: RVar[List[Position[List, Double]]] = Position.createPositions(domain, n)

      val points1: RVar[List[Position[List, Double]]] =
        points0.flatMap { l => l.traverse {
          solution => {
            val x = solution.pos
            val zipped = x zip domain.list
            val pbest = zipped.traverse { case (xi, di) => {
              val range = di.upper.value - di.lower.value
              val dev = range * 0.1
              Dist.gaussian(0.0, dev).map { g =>
                val add = xi + g
                if ((add > di.upper.value) || (add < di.lower.value))
                  xi - g
                else
                  add
              }
            }}
            pbest.map(Position(_))
          }
        }}

      val x0 = for {
        points <- points0
        steps = points.map(p => Step.evalF(p))
        sols <- steps.traverse(step => (step run opt)(eval))
      } yield sols

      val x1 = for {
        points <- points1
        steps = points.map(p => Step.evalF(p))
        sols <- steps.traverse(step => (step run opt)(eval))
      } yield sols

      val zipped = for {
        x0s <- x0
        x1s <- x1
      } yield (x0s zip x1s)

      val solutions: RVar[Maybe[List[Entity[Mem[List,Double], List, Double]]]] = zipped.map(_.traverse {
        case (xi, zi) =>
          for {
            xf <- xi.fit
            zf <- zi.fit
            xfv = xf.fold(_.v, _.v)
            zfv = zf.fold(_.v, _.v)
            (xi0, yi0) = if (zfv < xfv) (xi, zi) else (zi, xi)
          } yield Entity(Mem(yi0, xi0.zeroed), xi0)
      })

      val iteration0: RVar[Maybe[List[Particle[Mem[List,Double], List,Double]]]] = solutions.flatMap { m =>
        m.traverse { entities =>
          val psos0 = pso(entities)
          val s0 = entities.map(psos0)
          val iteration0 = s0.traverse(step => step.run(opt)(eval))
          iteration0
        }
      }

      val iteration1: RVar[Maybe[List[Particle[Mem[List,Double], List,Double]]]] = iteration0.flatMap { m =>
        m.traverse { entities =>
          val psos1 = pso(entities)
          val s1 = entities.map(psos1)
          val iteration1 = s1.traverse(step => step.run(opt)(eval))
          iteration1
        }
      }

      val iteration2: RVar[Maybe[List[Particle[Mem[List,Double], List,Double]]]] = iteration1.flatMap { m =>
        m.traverse { entities =>
          val psos2 = pso(entities)
          val s2 = entities.map(psos2)
          val iteration2 = s2.traverse(step => step.run(opt)(eval))
          iteration2
        }
      }

      for {
        m1 <- iteration1 // Maybe[List[Particle]]
        m2 <- iteration2 // Maybe[List[Particle]]
        zipped = (m1 zip m2) map { case (l1, l2) => l1 zip l2 } // Maybe[List(List, List)]
        filtered = zipped.map(l => l.filter { case (l1i, l2i) => inBounds(l2i.pos, domain) })
        l1 = filtered.map(l => l map { case (l1i, l2i) => l1i })
        l2 = filtered.map(l => l map { case (l1i, l2i) => l2i })
        f = (l1 zip l2) flatMap { case (i1, i2) => fci(i1.map(_.pos), i2.map(_.pos)) }
        length = l1.map(_.length.toDouble)
      } yield f

    }

  def inBounds(x: Position[List,Double], domain: NonEmptyList[Interval[Double]]) =
    (x.pos zip domain.list).forall {
      case (xi, di) => (xi >= di.lower.value) && (xi <= di.upper.value)
    }

  def fci(x: List[Position[List,Double]], y: List[Position[List,Double]]): Maybe[Double] = {

    def normFitness(solutions: List[Position[List, Double]]): Maybe[List[Double]] = {
      val best  = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run Min)
      val worst = solutions.reduceLeft((a, b) => Fitness.compare(a, b) run Max)

      for {
        b <- best.fit
        w <- worst.fit
        f <- x.traverse(_.fit)
        bv = b.fold(_.v, _.v)
        wv = w.fold(_.v, _.v)
        fv = f.map(_.fold(_.v, _.v))
      } yield fv.map(fi => (fi - wv) / (bv - wv))
    }

    for {
      nx <- normFitness(x)
      ny <- normFitness(y)
      zipped = nx zip ny
      gf = zipped.map { case (fi, fi1) => if (fi1 < fi) 1.0 else 0.0 }
    } yield gf.sum / gf.length.toDouble

  }

}
