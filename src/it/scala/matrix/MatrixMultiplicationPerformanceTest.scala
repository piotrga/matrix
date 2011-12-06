package matrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MatrixMultiplicationPerformanceTest extends FlatSpec with ShouldMatchers{
  "MyMatrix multiplication" should "be at least 2 times faster than apache implementation" in {

    val apache = runMultiplicationTest(ApacheMatrixOperations)
    val my = runMultiplicationTest(MyMatrixOperations)

    my.toDouble should be < (apache/2)
  }

  def runMultiplicationTest(ops: MatrixOperations) = {
    val M = random(500, 400, 1)
    val M1 = random(400, 250, 1)
    val M2 = random(250, 10, 1)
    val TIMES = 50

    val time_per_multiplication = benchmark(TIMES) { ops.multiply(ops.multiply (M.items, M1.items), M2.items) }
    println(ops.getClass.getSimpleName+": Time per multiplication= " + time_per_multiplication)
    time_per_multiplication
  }

}

