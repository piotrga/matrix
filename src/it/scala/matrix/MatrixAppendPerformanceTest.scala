package matrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//
//@RunWith(classOf[JUnitRunner])
class MatrixAppendPerformanceTest extends ShouldMatchers with FlatSpec {
  val X = random(5000, 100, 1)
  val rand = X.items.flatten

//  def loop {
//    val f: Double => Double = math.log
//    val res = printTime("Array.ofDim")(Array.ofDim[Double](500000))
//    var row = 0
//    var sum = 0d
//    val r = math.random
//    printTime("loop"){
//     (0 until 500000).par.foreach { row =>
//        res(row) = math.log(row)
//      }
//    }
//    res(0) = sum
//    sum
//  }
//
//  "log1" should  "be fast" in {
//    benchmark(100){
//      loop
//    } should be < 0.0
//  }

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