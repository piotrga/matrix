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
  "Matrix" should "multiply fast with default mechanism" in {
//    implicit val multiplyMatrix = Matrix.apacheMultiplyMatrix _
    import matrix._

    val M = random(500,400, 1)
    val M1 = random(400, 250, 1)

    val M2 = random(250, 10, 1)
    //    implicit val multiplication = mult1 _
    val TIMES = 100

    println("Time per multiplication= "+ time{
      (1 to TIMES).foreach(_ =>M * M1 * M2)
    }/TIMES)
  }

  "Matrix 5000x400x25x10" should "multiply fast" in {
//    implicit val multiplyMatrix = Matrix.apacheMultiplyMatrix _
    import matrix._

    val M = random(5000,400, 1)
    val M1 = random(400, 25, 1)
    val M2 = random(25, 10, 1)


    //    implicit val multiplication = mult1 _
    val TIMES = 100

    println("["+TIMES+"x] Total time of multiplication= "+ time{
      (1 to TIMES).foreach(_ =>M * M1 * M2)
    })
  }

}