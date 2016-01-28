package cilib

import scalaz._
import Scalaz._

object Runner {

  def repeat[F[_],A,B](n: Int, alg: Kleisli[Step[F,A,?],List[B],Result[B]], collection: RVar[List[B]]): Step[F,A,List[B]] = {
    Step.pointR(collection).flatMap(coll => (1 to n).toStream.foldLeftM[Step[F,A,?],List[B]](coll) { (a, c) =>
      alg.run(a).map(_.toList)
    })
  }

  def repeatS[F[_],A,B,S](n: Int, alg: Kleisli[StepS[F,A,S,?],List[B],Result[B]], collection: RVar[List[B]]): StepS[F,A,S,List[B]] = {
    StepS.pointR(collection).flatMap(coll => (1 to n).toStream.foldLeftM[StepS[F,A,S,?],List[B]](coll) { (a, c) =>
      alg.run(a).map(_.toList)
    })
  }

  def repeatA[F[_],A,B](algs: Seq[Kleisli[Step[F,A,?],List[B],Result[B]]], collection: RVar[List[B]]): Step[F,A,List[B]] = {
    Step.pointR(collection).flatMap(coll => algs.toStream.foldLeftM[Step[F,A,?],List[B]](coll) { (a, c) =>
      c.run(a).map(_.toList)
    })
  }

}
