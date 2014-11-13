package cilib

import org.scalacheck._

object SelectionTest extends Properties("Selections") {

 property("index neighbours") = {
   val ring = cilib.Selection.indexNeighbours[Int](3)
   ring(List(1), 1)             == List(1)
   ring(List(1, 2, 3), 2)       == List(1, 2, 3)
   ring(List(1, 2, 3, 4, 5), 3) == List(2, 3, 4)
   ring(List(1, 2, 3, 4, 5), 5) == List(4, 5, 1)
 }
    
}
