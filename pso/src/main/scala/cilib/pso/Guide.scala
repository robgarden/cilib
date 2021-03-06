package cilib
package pso

object Guide {

  def identity[S,F[_],A]: Guide[S,A] =
    (_, x) => Step.point(x.pos)

  def pbest[S,A](implicit M: Memory[S,A]): Guide[S,A] =
    (_, x) => Step.point(M._memory.get(x.state))

  def nbest[S](selection: Selection[Particle[S,Double]])(implicit M: Memory[S,Double]): Guide[S,Double] = {
    (collection, x) => Step.withCompare(o => RVar.point {
      val selected = selection(collection, x)
      val fittest = selected.map(e => M._memory.get(e.state)).reduceLeftOption((a, c) => Comparison.compare(a, c) run (o))
      fittest.getOrElse(sys.error("Impossible: reduce on entity memory worked on empty memory member"))
    })
  }

  def dominance[S](selection: Selection[Particle[S,Double]]): Guide[S,Double] = {
    (collection, x) => Step.withCompare(o => RVar.point {
      val neighbourhood = selection(collection, x)
      val comparison = Comparison.dominance(o.opt)
      val fittest = neighbourhood.map(_.pos).reduceLeftOption((a,c) => comparison.apply(a, c))
      fittest.getOrElse(sys.error("????"))
    })
  }

  def gbest[S](implicit M: Memory[S,Double]): Guide[S,Double] =
    nbest((c, _) => c)

  def lbest[S](n: Int)(implicit M: Memory[S,Double]) =
    nbest(Selection.indexNeighbours[Particle[S,Double]](n))

}
