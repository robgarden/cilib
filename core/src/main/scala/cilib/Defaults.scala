package cilib

import _root_.scala.Predef.{any2stringadd => _, _}
import scalaz._
import Scalaz._
import PSO._
import spire.algebra.{Order => _, _}
import spire.implicits._
import spire.syntax.module._

object Defaults {

  // The function below needs the guides for the particle, for the standard PSO update
  // and will eventually live in the simulator
  def gbest[S,F[_]:Traverse](
    w: Double,
    c1: Double,
    c2: Double,
    cognitive: Guide[S,F,Double],
    social: Guide[S,F,Double]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      cog     <- cognitive(collection, x)
      soc     <- social(collection, x)
      v       <- stdVelocity(x, soc, cog, w, c1, c2)
      p       <- stdPosition(x, v)
      p2      <- evalParticle(p)
      p3      <- updateVelocity(p2, v)
      updated <- updatePBest(p3)
    } yield One(updated)

  def arpso[S,F[_]:Traverse](
    w: Double,
    c1: Double,
    c2: Double,
    l: Double,
    dlow: Double,
    dhigh: Double,
    cognitive: Guide[S,F,Double],
    social: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => StateT[Step[F,Double,?], ARPSOParams, Result[Particle[S,F,Double]]] =
    collection => x => {
      val S = StateT.stateTMonadState[ARPSOParams, Step[F,Double,?]]
      val hoist = StateT.StateMonadTrans[ARPSOParams]

      for {
        p       <- hoist.liftMU(evalParticle(x))
        p1      <- hoist.liftMU(updatePBestIfInBounds(p, bounds))
        cog     <- hoist.liftMU(cognitive(collection, p1))
        soc     <- hoist.liftMU(social(collection, p1))
        div     <- hoist.liftMU(diversity(collection, l))
        state   <- S.get
        dir      = state.dir
        sign     = if (dir > 0 && div < dlow) -1.0 else if (dir < 0 && div > dhigh) 1.0 else dir
        v       <- hoist.liftMU(stdVelocity(p1, soc, cog, w, sign * c1, sign * c2))
        p2      <- hoist.liftMU(stdPosition(p1, v))
        updated <- hoist.liftMU(updateVelocity(p2, v))
        _       <- S.put(ARPSOParams(sign))
      } yield One(updated)
    }

  def gbestBounded[S,F[_]:Traverse](
    w: Double,
    c1: Double,
    c2: Double,
    cognitive: Guide[S,F,Double],
    social: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      p       <- evalParticle(x)
      p1      <- updatePBestIfInBounds(p, bounds)
      cog     <- cognitive(collection, p1)
      soc     <- social(collection, p1)
      v       <- stdVelocity(p1, soc, cog, w, c1, c2)
      p2      <- stdPosition(p1, v)
      updated <- updateVelocity(p2, v)
    } yield One(updated)

  def bb[S,F[_]:Traverse:Zip](
    social: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      p       <- evalParticle(x)
      p1      <- updatePBestIfInBounds(p, bounds)
      soc     <- social(collection, p1)
      v       <- barebones(p1, soc)
      p2      <- replace(p1, v)
      updated <- updateVelocity(p2, v)
    } yield One(updated)

  def bbe[S,F[_]:Traverse:Zip](
    threshold: Double,
    social: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      p       <- evalParticle(x)
      p1      <- updatePBestIfInBounds(p, bounds)
      soc     <- social(collection, p1)
      v       <- barebonesExploit(p1, soc, threshold)
      p2      <- replace(p1, v)
      updated <- updateVelocity(p2, v)
    } yield One(updated)

def constrictionPSO[S,F[_]:Traverse](
  X: Double,
  guideStrategies: List[(Guide[S,F,Double],Double,RVar[Double])],
  bounds: F[Interval[Double]]
)(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
  collection => x => for {
    p0      <- evalParticle(x)
    p01     <- updatePBestIfInBounds(p0, bounds)

    guides  <- guideStrategies.map { case (gs, _, _) => gs(collection, x) }.sequenceU
    guidesAndCo = guides.zip(guideStrategies.map(_._2)).zip(guideStrategies.map(_._3)).map { case ((g,c),r) => (g,c,r) }
    v       <- velocityWithConstriction(x, guidesAndCo, X)
    p       <- stdPosition(p01, v)
    p2      <- evalParticle(p)
    p3      <- updateVelocity(p2, v)
    updated <- updatePBestIfInBounds(p3, bounds)
  } yield cilib.One(updated)

  def constrictionPSONoCo[S,F[_]:Traverse](
    X: Double,
    guideStrategies: List[Guide[S,F,Double]],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      // doing these two steps first resolves particles immediately flying off into space
      // must re look at original PSO
      p0  <- evalParticle(x)
      p01 <- updatePBestIfInBounds(p0, bounds)

      guides  <- guideStrategies.traverseU(gs => gs(collection, x))
      v       <- velocityWithConstrictionNoCo(x, guides, X)
      p       <- stdPosition(p01, v)
      p2      <- evalParticle(p)
      p3      <- updateVelocity(p2, v)
      updated <- updatePBestIfInBounds(p3, bounds)
    } yield cilib.One(updated)

  def nmpco[S,F[_]:Traverse](
    guide: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      p       <- evalParticle(x)
      p1      <- updatePBestIfInBounds(p, bounds)
      co      <- guide(collection, p1)
      p2      <- replace(p1, co)
      evalP   <- evalParticle(p2)
      best    <- better(p1, evalP)
    } yield One(best)

  def cognitive[S,F[_]:Traverse](
    w: Double,
    c1: Double,
    cognitive: Guide[S,F,Double]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Particle[S,F,Double]] =
    collection => x => {
      for {
        cog     <- cognitive(collection, x)
        v       <- singleComponentVelocity(x, cog, w, c1)
        p       <- stdPosition(x, v)
        p2      <- evalParticle(p)
        p3      <- updateVelocity(p2, v)
        updated <- updatePBest(p3)
      } yield updated
    }

  def social[S,F[_]:Traverse](
    w: Double,
    c1: Double,
    social: Guide[S,F,Double]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Particle[S,F,Double]] =
    collection => x => {
      for {
        soc     <- social(collection, x)
        v       <- singleComponentVelocity(x, soc, w, c1)
        p       <- stdPosition(x, v)
        p2      <- evalParticle(p)
        p3      <- updateVelocity(p2, v)
        updated <- updatePBest(p3)
      } yield updated
    }

  def pcxPSO[S,F[_]:Traverse](
    guide: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      // doing these two steps first resolves particles immediately flying off into space
      // must re look at original PSO
      p0      <- evalParticle(x)
      p01     <- updatePBestIfInBounds(p0, bounds)
      g       <- guide(collection, p01)
      p       <- updateVelocity(p01, g)
      updated <- replace(p, g)
      //p2      <- evalParticle(p)
      //p3      <- updateVelocity(p2, g)
      //updated <- updatePBestIfInBounds(p3, bounds)
    } yield cilib.One(updated)

  def pcxPSOPrev[S,F[_]:Traverse](
    guide: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], P: Previous[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => for {
      // doing these two steps first resolves particles immediately flying off into space
      // must re look at original PSO
      p0      <- evalParticle(x)
      p01     <- updatePBestIfInBounds(p0, bounds)
      g       <- guide(collection, p01)
      p       <- updateVelocity(p01, g)
      updated <- positionWithPrev(p, g)
      //p2      <- evalParticle(p)
      //p3      <- updateVelocity(p2, g)
      //updated <- updatePBestIfInBounds(p3, bounds)
    } yield cilib.One(updated)

  def pcxPSODeb[S,F[_]:Traverse:Zip](
    w: Double, c1: Double, c2: Double, s1: Double, s2: Double, rr: Int,
    selection: Selection[Entity[S,F,Double]],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], P: Previous[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => {
      val gb = Guide.gbest
      val pb = Guide.pbest
      val prev = Guide.previous
      def repeater(parents: NonEmptyList[Position[F,Double]]) = Guide.pcxRepeater(s1, s2, rr, parents, selection)

      implicit def positionOrder: Order[Position[F,Double]] = Order.orderBy(_.magnitude)

      for {
        p0      <- evalParticle(x)
        p01     <- updatePBestIfInBounds(p0, bounds)

        gbest   <- gb(collection, p01)
        pbest   <- pb(collection, p01)
        pos     <- Step.point(p01.pos)
        pre     <- prev(collection, p01)

        parents  = NonEmptyList(gbest, pbest, pos)
        parents1 = NonEmptyList(gbest, pbest, pos, pre)

        foundDistinct = (parents1.distinct.size >= 2)

        g       <-
        if (parents.distinct.size == 3) repeater(parents)(collection, x)
        else if (parents1.distinct.size >= 2) repeater(parents1)(collection, x)
        else stdVelocity(p01, gbest, pbest, w, c1, c2)

        p <- updateVelocity(p01, g)
        posPrev <- positionWithPrev(p, g)
        updated <- if (!foundDistinct) stdPosition(posPrev, g) else positionWithPrev(p, g)
      } yield cilib.One(updated)
    }

  def pcxPSORepeatingNoise[S,F[_]:Traverse:Zip](
    s1: Double, s2: Double, rr: Int,
    selection: Selection[Entity[S,F,Double]],
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => {
      val gb = Guide.gbest
      val pb = Guide.pbest
      def repeater(parents: NonEmptyList[Position[F,Double]]) = Guide.pcxRepeater(s1, s2, rr, parents, selection)

      implicit def positionOrder: Order[Position[F,Double]] = Order.orderBy(_.magnitude)

      for {
        p0      <- evalParticle(x)
        p01     <- updatePBestIfInBounds(p0, bounds)

        gbest   <- gb(collection, p01)
        pbest   <- pb(collection, p01)
        pos      = p01.pos

        parents       = NonEmptyList(gbest, pbest, pos)
        foundDistinct = parents.distinct.size == 3

        g <- if (foundDistinct) repeater(parents)(collection, x)
             else for {
               mutated   <- mutateNoise(p01.pos)
               child     <- Step.evalF(mutated)
               pbestNew  <- betterPos(p01.pos, child)
               posNew    <- worsePos(p01.pos, child)
               parentsNew = NonEmptyList(gbest, pbestNew, posNew)
               pcxd      <- repeater(parentsNew)(collection, p01)
             } yield pcxd

        updated <- replace(p01, g)
      } yield cilib.One(updated)
    }

  def pcxBoltzmanPSONoise[S,F[_]:Traverse:Zip](s1: Double, s2: Double, temp: Double, bounds: F[Interval[Double]])(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => {
      val gb = Guide.gbest
      val pb = Guide.pbest
      val pcx = Crossover.pcx[F](s1, s2)
      val boltzmann = Selection.boltzmann[F](temp)

      implicit def positionOrder: Order[Position[F,Double]] = Order.orderBy(_.magnitude)

      for {
        p0      <- evalParticle(x)
        p01     <- updatePBestIfInBounds(p0, bounds)

        gbest   <- gb(collection, p01)
        pbest   <- pb(collection, p01)
        pos      = p01.pos

        parents  = NonEmptyList(gbest, pbest, pos)
        foundDistinct = parents.distinct.size == 3

        g <- if (foundDistinct) pcx(parents).flatMap(child => boltzmann(child, gbest).map(_.getOrElse(gbest)))
             else for {
               mutated   <- mutateNoise(p01.pos)
               child     <- Step.evalF(mutated)
               pbestNew  <- betterPos(p01.pos, child)
               posNew    <- worsePos(p01.pos, child)
               parentsNew = NonEmptyList(gbest, pbestNew, posNew)
               pcxd      <- pcx(parentsNew).flatMap(child => boltzmann(child, gbest).map(_.getOrElse(gbest)))
             } yield pcxd

        updated <- replace(p01, g)
      } yield cilib.One(updated)
    }

  def pcxBoltzmanPSODeb[S,F[_]:Traverse:Zip](
    w: Double, c1: Double, c2: Double, s1: Double, s2: Double, temp: Double,
    bounds: F[Interval[Double]]
  )(implicit M: Memory[S,F,Double], V: Velocity[S,F,Double], P: Previous[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Result[Particle[S,F,Double]]] =
    collection => x => {
      val gb = Guide.gbest
      val pb = Guide.pbest
      val prev = Guide.previous
      val pcx = Crossover.pcx[F](s1, s2)
      val boltzmann = Selection.boltzmann[F](temp)

      implicit def positionOrder: Order[Position[F,Double]] = Order.orderBy(_.magnitude)

      for {
        p0      <- evalParticle(x)
        p01     <- updatePBestIfInBounds(p0, bounds)

        gbest   <- gb(collection, p01)
        pbest   <- pb(collection, p01)
        pos     <- Step.point(p01.pos)
        pre     <- prev(collection, p01)

        parents  = NonEmptyList(gbest, pbest, pos)
        parents1 = NonEmptyList(gbest, pbest, pos, pre)

        foundDistinct = parents1.distinct.size >= 2

        g       <-
        if (parents.distinct.size == 3) pcx(parents).flatMap(child => boltzmann(child, gbest).map(_.getOrElse(gbest)))
        else if (parents1.distinct.size >= 2) pcx(parents1).flatMap(child => boltzmann(child, gbest).map(_.getOrElse(gbest)))
        else stdVelocity(p01, gbest, pbest, w, c1, c2)

        p <- updateVelocity(p01, g)
        posPrev <- positionWithPrev(p, g)
        updated <- if (!foundDistinct) stdPosition(posPrev, g) else positionWithPrev(p, g)
      } yield cilib.One(updated)
    }

  // This is only defined for the gbest topology because the "method" described in Edwin's
  // paper for alternate topologies _does not_ make sense. I can only assume that there is
  // some additional research that needs to be done to correctly create an algorithm to
  // apply gcpso to other topology structures. Stating that you simply "copy" something
  // into something else is not elegant and does not have a solid reasoning
  // attached to it.
  def gcpso[S,F[_]:Traverse](
    w: Double,
    c1: Double,
    c2: Double,
    cognitive: Guide[S,F,Double],
    bounds: F[Interval[Double]]
  )(
    implicit M:Memory[S,F,Double], V:Velocity[S,F,Double],mod: Module[F[Double],Double]
  ): List[Particle[S,F,Double]] => Particle[S,F,Double] => StateT[Step[F,Double,?], GCParams, Result[Particle[S,F,Double]]] =
    collection => x => {
      val S = StateT.stateTMonadState[GCParams, Step[F,Double,?]]
      val hoist = StateT.StateMonadTrans[GCParams]
      val g = Guide.gbest[S,F,Double]
      for {
        gbest   <- hoist.liftMU(g(collection, x))
        cog     <- hoist.liftMU(cognitive(collection, x))
        isBest  <- hoist.liftMU(Step.point[F,Double,Boolean](x.pos eq gbest))
        s       <- S.get
        v       <- hoist.liftMU(if (isBest) gcVelocity(x, gbest, w, s) else stdVelocity(x, gbest, cog, w, c1, c2)) // Yes, we do want reference equality
        p       <- hoist.liftMU(stdPosition(x, v))
        p2      <- hoist.liftMU(evalParticle(p))
        p3      <- hoist.liftMU(updateVelocity(p2, v))
        updated <- hoist.liftMU(updatePBestIfInBounds(p3, bounds))
        failure <- hoist.liftMU(Step.liftK[F,Double,Boolean](Fitness.compare(x.pos, updated.pos) map (_ eq x.pos)))
        _       <- S.modify(params =>
          if (isBest) {
            params.copy(
              p = if (params.successes > params.e_s) 2.0*params.p else if (params.failures > params.e_f) 0.5*params.p else params.p,
              failures = if (failure) params.failures + 1 else 0,
              successes = if (!failure) params.successes + 1 else 0
            )
          } else params)
      } yield One(updated)
    }

  def charged[S:Charge,F[_]:Traverse](
    w: Double,
    c1: Double,
    c2: Double,
    cognitive: Guide[S,F,Double],
    social: Guide[S,F,Double],
    distance: (Position[F,Double], Position[F,Double]) => Double,
    rp: Double,
    rc: Double
  )(implicit M:Memory[S,F,Double], V:Velocity[S,F,Double], MO: Module[F[Double],Double]): List[Particle[S,F,Double]] => Particle[S,F,Double] => Step[F,Double,Particle[S,F,Double]] =
    collection => x => for {
      cog     <- cognitive(collection, x)
      soc     <- social(collection, x)
      accel   <- acceleration(collection, x, distance, rp, rc)
      v       <- stdVelocity(x, soc, cog, w, c1, c2)
      p       <- stdPosition(x, v + accel)
      p2      <- evalParticle(p)
      p3      <- updateVelocity(p2, v)
      updated <- updatePBest(p3)
    } yield updated


}
