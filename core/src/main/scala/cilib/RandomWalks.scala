package cilib

import scalaz._
import Scalaz._

object RandomWalks {

  type Walk = List[Position[List, Double]]
  type StartingZones = List[Boolean]
  type WalkStep = Position[List, Double]

  def progressive(domain: NonEmptyList[Interval[Double]], steps: Int, stepSize: Double) = {

    val n = domain.size

    // random bits indicating starting zone per dimension
    val start: RVar[StartingZones] =
      Range.inclusive(0, n - 1).toList.traverse(i => RVar.next[Boolean])
      // (0 until n - 1).toList.toNel.map(_.traverse(i => RVar.next[Boolean]))

    val zipped: RVar[List[((Boolean, Interval[Double]), Int)]] =
      start.map(s => s.zip(domain.list).zipWithIndex)

    val walk0: RVar[WalkStep] = zipped.flatMap(z => z.traverse {
      case ((b, di), i) =>
        val max = di.upper.value
        val min = di.lower.value
        val r = Dist.uniform(0.0, (max - min) / 2.0)
        val walk0i = if (b) r.map(max - _) else r.map(min + _)
        val randDim: RVar[Int] = Dist.uniformInt(0, n - 1)
        randDim.flatMap(rD =>
          if (rD == i) {
            if (b) walk0i.map(_ => max) else walk0i.map(_ => min)
          } else walk0i
       )
    }).map(Position(_))

    val S = StateT.stateTMonadState[(StartingZones, WalkStep, Int), RVar]
    val hoist = StateT.StateMonadTrans[(StartingZones, WalkStep, Int)]

    def doWalk: StateT[RVar, (StartingZones, WalkStep, Int), WalkStep] =
      for {
        state <- S.get
        (zones, awalk, step) = state
        w <- hoist.liftM((zones zip awalk.pos zip domain.list).traverse {
          case ((s1, ws1), i) =>
              val r: RVar[Double] = Dist.uniform(0.0, stepSize).map(ri => if (s1) -ri else ri)
              val wss: RVar[(Boolean, Double, Interval[Double])] = r.map(ws1 + _).map { wss =>
                if (wss < i.lower.value) (!s1, i.lower.value + (i.lower.value - wss), i)
                else if (wss > i.upper.value) (!s1, i.upper.value - (wss - i.upper.value), i)
                else (s1, wss, i)
              }
              wss
        })
        newZone = w.map(_._1)
        newPos  = Position(w.map(_._2))
        _ <- S.put((newZone, newPos, step + 1))
      } yield newPos

    (start |@| walk0) { (_, _, 1)} flatMap {
      state => doWalk.replicateM(steps - 1).run(state).map(state._2 :: _._2)
    }
  }

  def progressiveManhattan(domain: NonEmptyList[Interval[Double]], steps: Int, stepSize: Double) = {

    val n = domain.size

    // random bits indicating starting zone per dimension
    val start: RVar[StartingZones] =
      Range.inclusive(0, n - 1).toList.traverse(i => RVar.next[Boolean])

    val zipped: RVar[List[((Boolean, Interval[Double]), Int)]] =
      start.map(s => s.zip(domain.list).zipWithIndex)

    val walk0: RVar[WalkStep] = zipped.flatMap(z => z.traverse {
      case ((b, di), i) =>
        val max = di.upper.value
        val min = di.lower.value
        val r = Dist.uniform(0.0, (max - min) / 2.0)
        val walk0i = if (b) r.map(max - _) else r.map(min + _)
        val randDim: RVar[Int] = Dist.uniformInt(0, n - 1)
        randDim.flatMap(rD =>
            if (rD == i) {
              if (b) walk0i.map(_ => max) else walk0i.map(_ => min)
              } else walk0i)
    }).map(Position(_))

    val S = StateT.stateTMonadState[(StartingZones, WalkStep, Int), RVar]
    val hoist = StateT.StateMonadTrans[(StartingZones, WalkStep, Int)]

    def doWalk: StateT[RVar, (StartingZones, WalkStep, Int), WalkStep] =
      for {
        state <- S.get
        (zones, awalk, step) = state
        r <- hoist.liftM(Dist.uniformInt(0, n - 1))
        w = ((zones zip awalk.pos zip domain.list).zipWithIndex).map {
          case (((s1, ws1), interval), i)  =>
            val wss = {
              if (i == r) {
                val inc = if (s1 == true) -1 else 1
                val wsRD = ws1 + (inc * stepSize)
                if ((wsRD < interval.lower.value) || (wsRD > interval.upper.value)) (!s1, ws1 - (inc * stepSize), interval)
                else (s1, wsRD, interval)
              } else {
                (s1, ws1, interval)
              }
            }
            wss
        }
        newZone = w.map(_._1)
        newPos  = Position(w.map(_._2))
        _ <- S.put((newZone, newPos, step + 1))
      } yield newPos

    (start |@| walk0) { (_, _, 1)} flatMap {
      state => doWalk.replicateM(steps - 1).run(state).map(state._2 :: _._2)
    }
  }
}
