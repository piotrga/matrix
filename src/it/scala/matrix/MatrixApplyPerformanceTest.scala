package matrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MatrixApplyPerformanceTest extends FlatSpec with ShouldMatchers{

  "MyMatrixOperations" should "do apply fast for 10x10" in { speed_ratio_for_dimentions(10, 10, new MatrixOperations() {}, MyMatrixOperations) should be < 1.2}
  it should "do apply fast for 200x200" in     { speed_ratio_for_dimentions(200, 200, new MatrixOperations() {}, MyMatrixOperations) should be < 0.9}
  it should "do apply fast for 1000x100" in    { speed_ratio_for_dimentions(1000, 100, new MatrixOperations() {}, MyMatrixOperations) should be < 0.7}
  it should "do apply fast for 100x1000" in    { speed_ratio_for_dimentions(100, 1000, new MatrixOperations() {}, MyMatrixOperations) should be < 0.7}
  it should "do apply fast for 1000x1" in      { speed_ratio_for_dimentions(1000, 1, new MatrixOperations() {}, MyMatrixOperations) should be < 1.3}
  it should "do apply fast for 10x1000" in     { speed_ratio_for_dimentions(10, 1000, new MatrixOperations() {}, MyMatrixOperations) should be < 1.1}

  "ActorBasedMatrixOperations" should "do apply fast for 10x1000" in     { speed_ratio_for_dimentions(10, 1000, MyMatrixOperations, ActorBasedMatrixOperations) should be < 0.5d}


  def speed_ratio_for_dimentions(rows: Int, columns: Int, operations1: MatrixOperations, operations2: MatrixOperations) ={
    val M = random(rows, columns, 1)
    val N = math.max(1000 * 100 * 100 / (rows*columns*2), 1)

    val function = (x:Double) => x * 12345678.91011

    val items = M.items
    var time_of_unparallel = benchmark(N)(operations1.apply(items, function))
    var time_of_my = benchmark(N)(M(function))
    // we are dropping first results as i7 processors need some time before they switch to turbo mode
    time_of_unparallel = math.min(time_of_unparallel, benchmark(N)(operations1.apply(items, function)))
    time_of_my = math.min(time_of_my, benchmark(N)(operations2.apply(items, function)))

    //    l("Unparallel(%dx%d)[%d]: %g", rows,columns, N, time_of_unparallel)
    //    l("My: %g", time_of_my)

    time_of_my/time_of_unparallel
  }

}

