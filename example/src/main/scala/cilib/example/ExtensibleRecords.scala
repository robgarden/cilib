package cilib
package example

import scalaz.effect._
import scalaz.effect.IO.putStrLn
import scalaz.std.list._

import shapeless._
import shapeless.record._
import shapeless.syntax.singleton._

object ExtensibleRecords extends SafeApp {


  val record = ("Name" ->> "Robert") :: ("Age" ->> 26) :: HNil

  override val runc: IO[Unit] =
    putStrLn(record.keys.toString)
}

