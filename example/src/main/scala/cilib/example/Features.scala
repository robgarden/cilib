package cilib
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn
import scalaz.std.list._

import shapeless._
import shapeless.record._
import shapeless.syntax.singleton._

object Features extends SafeApp {

  case class Feature[A](name: String, gen: RVar[A])

  val plainFeatures = List(
    "age int 0 120",
    "salary double 0 200000",
    "work categorical it financial academic government",
    "retired bool"
  )

  val regex = """^(\w*)\s*(\w*)\s*(.*)$""".r

  val attributes = plainFeatures.map {
    _ match {
      case regex(name, t, values) => toFeature(name, t, values)
      case _ => None
    }
  }

  override val runc: IO[Unit] =
    putStrLn(attributes.toString)

  def toFeature(name: String, t: String, values: String) =
    t match {
      case "int"         => toIntFeature(name, values)
      case "double"      => toDoubleFeature(name, values)
      case "categorical" => toCategoricalFeature(name, values)
      case "bool"        => toBoolFeature(name)
    }

  def toIntFeature(name: String, values: String): Option[Feature[Int]] = {
    val split = values.split(" ").map(_.toInt)
    if (split.length != 2) None
    else Some(Feature(name, Dist.uniformInt(split.head, split.last)))
  }

  def toDoubleFeature(name: String, values: String): Option[Feature[Double]] = {
    val split = values.split(" ").map(_.toDouble)
    if (split.length != 2) None
    else Some(Feature(name, Dist.uniform(split.head, split.last)))
  }

  def toCategoricalFeature(name: String, values: String): Option[Feature[String]] = {
    val split = values.split(" ")
    if (split.length < 1) None
    else Some(Feature(name, RVar.choose(NonEmptyList(split.head, split.tail:_*))))
  }

  def toBoolFeature(name: String): Option[Feature[Boolean]] =
    Some(Feature(name, RVar.next[Boolean]))

}

