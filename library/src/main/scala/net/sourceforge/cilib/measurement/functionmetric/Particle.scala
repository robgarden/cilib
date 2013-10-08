package net.sourceforge.cilib.measurement.functionmetric

object Particle {
  def apply(curr: Point): Particle = {
    apply(curr, curr)
  }

  def apply(curr: Point, pbest: Point): Particle = {
    val velocity: Array[Double] = Array.fill(curr.x.size)(0)
    new Particle(curr, pbest, velocity)
  }

  def apply(curr: Point, pbest: Point, vel: Array[Double]): Particle =
    new Particle(curr, pbest, vel)
}

class Particle(
  val curr: Point,
  val pbest: Point,
  val velocity: Array[Double])