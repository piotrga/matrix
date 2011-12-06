package matrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MatrixApplyPerformanceTest extends ShouldMatchers with FlatSpec {
  val X = random(5000, 100, 1)
  val rand = X.items.flatten

  "log" should  "be fast" in {
    benchmark(100){
      X(math.log)
    } should be < 10d
  }

  "+" should  "be fast" in {
    benchmark(100){
      X + 1
    } should be < 5d
  }


  "@^" should  "be fast" in {
    benchmark(100){
      X@^2
    } should be < 7d
  }

  "Matrix" should  "do :: fast" in {
    benchmark(100){
      1::X
    } should be < 4.0
  }
  "Matrix" should  "drop first column fast" in {
    benchmark(100){
      X.dropFirstColumn
    } should be < 7d
  }




}