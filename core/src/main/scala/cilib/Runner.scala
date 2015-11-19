package cilib

import scalaz._
import Scalaz._

object Runner {

  def repeat[F[_],A,B](n: Int, alg: Kleisli[Step[F,A,?],List[B],Result[B]], collection: RVar[List[B]]): Step[F,A,List[B]] = {
    Step.pointR(collection).flatMap(coll => (1 to n).toStream.foldLeftM[Step[F,A,?],List[B]](coll) { (a, c) =>
      alg.run(a).map(_.toList)
    })
  }

  // (Iteration, Swarm) => C
  type Measure[B,C] = (Int,List[B]) => C

  def repeatWithMeasurement[F[_],A,B,C](n: Int, alg: Kleisli[Step[F,A,?],List[B],Result[B]],
    collection: RVar[List[B]],
    measure: Measure[B,C]): Step[F,A,List[C]] = {

    Step.pointR(collection).flatMap(coll =>
        (1 to n).toStream.foldLeftM[Step[F,A,?],(List[B], List[C])]((coll, List())) { (a, c) =>
          alg.run(a._1).map(l => (l.toList, a._2 :+ measure(c, l.toList)))
    }).map(_._2)
  }
}
