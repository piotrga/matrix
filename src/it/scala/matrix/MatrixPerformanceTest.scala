package matrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

//import org.junit.runner.RunWith
//import org.scalatest.junit.JUnitRunner
//
//@RunWith(classOf[JUnitRunner])
class MatrixPerformanceTest extends FlatSpec with ShouldMatchers{
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

  "Matrix multiplication" should "be at least 2 times faster than apache implementation" in {


    def runTest(ops: MatrixOperations) = {
      val M = random(500, 400, 1)
      val M1 = random(400, 250, 1)
      val M2 = random(250, 10, 1)
      val TIMES = 50

      val time_per_multiplication = benchmark(TIMES) { ops.multiply(ops.multiply (M.items, M1.items), M2.items) }
      println("Time per multiplication= " + time_per_multiplication)
      time_per_multiplication
    }

    def apache = {
      runTest(ApacheMatrixOperations)
    }


    def my = {
      runTest(MyMatrixOperations)
    }

    my.toDouble should be < (apache/2)
  }



  def l(s:String, args:Any*) { println(">>>"+s.format(args:_*))}


  def speed_ratio_for_dimentions(rows: Int, columns: Int) ={
    val M = random(rows, columns, 1)
    val N = math.max(1000 * 100 * 100 / (rows*columns*2), 1)

    val unparallelOperations = new MatrixOperations() {}
    val myOperations = MyMatrixOperations

    val function = (x:Double) => x * 12345678.91011

    val items = M.items
    var time_of_unparallel = benchmark(N)(unparallelOperations.apply(items, function))
    var time_of_my = benchmark(N)(M(function))
    // we are dropping first results as i7 processors need some time before they switch to turbo mode
    time_of_unparallel = math.min(time_of_unparallel, benchmark(N)(unparallelOperations.apply(items, function)))
    time_of_my = math.min(time_of_my, benchmark(N)(M(function)))

//    l("Unparallel(%dx%d)[%d]: %g", rows,columns, N, time_of_unparallel)
//    l("My: %g", time_of_my)

    time_of_my/time_of_unparallel
  }

  "Matrix" should "do scalar operations fast 10x10" in { speed_ratio_for_dimentions(10, 10) should be < 1.2}
  it should "do scalar operations fast 200x200" in     { speed_ratio_for_dimentions(200, 200) should be < 0.9}
  it should "do scalar operations fast 1000x100" in    { speed_ratio_for_dimentions(1000, 100) should be < 0.7}
  it should "do scalar operations fast 100x1000" in    { speed_ratio_for_dimentions(100, 1000) should be < 0.7}
  it should "do scalar operations fast 1000x1" in      { speed_ratio_for_dimentions(1000, 1) should be < 1.3}
  it should "do scalar operations fast 10x1000" in     { speed_ratio_for_dimentions(10, 1000) should be < 1.1}
  it should  "do :: fast" in {
    val X = random(5000, 100, 1)
    benchmark(1000){
      1::X
    } should be < 1.0
  }

}