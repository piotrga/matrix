package matrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


class MatrixMultiplicationTest extends FlatSpec with ShouldMatchers{
  def doWithin(millis:Long)( block : => Unit){
    val start = System.currentTimeMillis()
    block
    val duration = System.currentTimeMillis() - start
    if (duration > millis) throw new Exception("Timeout of %d ms exceeded by %d ms!".format(millis, duration-millis))
  }

  def time( block : => Unit):Long = {
    val start = System.currentTimeMillis()
    block
    System.currentTimeMillis() - start
  }

  "Matrix" should "multiplication should be at least 4 times faster than apache implementation" in {


    def runTest(ops: MatrixOperations) = {
      val M = random(500, 400, 1)
      val M1 = random(400, 250, 1)
      val M2 = random(250, 10, 1)
      val TIMES = 50

      val time_per_multiplication = time {
        (1 to TIMES).foreach(_ => ops.multiply(ops.multiply (M.items, M1.items), M2.items))
      } / TIMES
      println("Time per multiplication= " + time_per_multiplication)
      time_per_multiplication
    }

    def apache = {
      runTest(ApacheMatrixOperations)
    }

    def my = {
      runTest(MyMatrixOperations)
    }
//    my
//    apache

    my should be < (apache/4)
  }

}