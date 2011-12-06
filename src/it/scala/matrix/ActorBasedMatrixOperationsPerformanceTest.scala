package matrix

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec


class ActorBasedMatrixOperationsPerformanceTest extends FlatSpec with ShouldMatchers{

  def benchmark(iterations:Int)( block : => Unit):Double = {
    val start = System.nanoTime()
    var i = 0
    while(i<iterations) {block;i+=1}
    val res = ((System.nanoTime() - start) /1000000).toDouble/ iterations
    res
  }

  def runMultiplicationTest(ops: MatrixOperations) = {
    val M = Array.fill(500, 400)(math.random)
    val M1 = Array.fill(400, 250)(math.random)
    val M2 = Array.fill(250, 10)(math.random)
    val TIMES = 50

    val time_per_multiplication = benchmark(TIMES) { ops.multiply(ops.multiply (M, M1), M2) }
    println(ops.getClass.getSimpleName+": Time per multiplication = "+  time_per_multiplication)
    time_per_multiplication
  }

  "ActorBasedMatrix multiplication" should "be comparable to parallel collection multiplication" in {

    val actor = runMultiplicationTest(ActorBasedMatrixOperations)
    val my = runMultiplicationTest(MyMatrixOperations)

    actor.toDouble/my should be < 1.2
  }
}