package application.ex4

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import matrix._

class Ex4Test extends FlatSpec with ShouldMatchers{

  lazy val X = Matrix.fromFile("data/X.txt")
  lazy val y = Matrix.fromFile("data/Y.txt")
  lazy val Y = Ex4.numbersToBitmaps(y, 10)
  
  lazy val thetas : RowVector = Matrix.fromFile("data/theta.txt").T
  val layerDim = List((25, 401), (10, 26))


  "CostFunction" should "convert numbers to 01 vectors" in {
    Ex4.numbersToBitmaps(Vector(10,1,2,3,4,5,6,7,8,9), 10) should be (Matrix(
      (0,0,0,0,0,0,0,0,0,1),
      (1,0,0,0,0,0,0,0,0,0),
      (0,1,0,0,0,0,0,0,0,0),
      (0,0,1,0,0,0,0,0,0,0),
      (0,0,0,1,0,0,0,0,0,0),
      (0,0,0,0,1,0,0,0,0,0),
      (0,0,0,0,0,1,0,0,0,0),
      (0,0,0,0,0,0,1,0,0,0),
      (0,0,0,0,0,0,0,1,0,0),
      (0,0,0,0,0,0,0,0,1,0)
    ))
  }


  it should "calculate correct cost without regularization" in {
    new J(X,Y, layerDim)(lambda = 0).cost(thetas) should be (0.287629 plusOrMinus 0.0000005)
  }

  it should "calculate correct cost with regularization" in {
    new J(X,Y, layerDim)(lambda = 1).cost(thetas) should be (0.383770 plusOrMinus 0.0000005)

  }

  def numericGradient(j: RowVector=>Double, thetas: RowVector) :RowVector= {
    val e = 1e-4
    val numgrad = Array.fill(thetas.cols)(0.0)
    val perturb = Array.fill(thetas.cols)(0.0)
    (0 until thetas.cols).foreach{ p =>
    //      printTime("Numeric gradient "+p)
    {
      perturb(p) = e
      val loss1 = j(thetas - RowVector(perturb))
      val loss2 = j(thetas + RowVector(perturb))
      numgrad(p) = (loss2 - loss1) / (2 * e)
      if(p % 10 == 0) println(100.0* p/thetas.cols+"%")
      perturb(p) = 0
    }

    }
    RowVector(numgrad)
  }


  "norm2" should "calculate" in {
    norm2(RowVector(2, -2)) should be (math.sqrt(8) plusOrMinus  1e-10)
  }




  "Gradient" should "be close to numeric gradient" in {
//    val T1 = Matrix(
//      (0.084147,-0.027942,-0.099999,-0.028790)
//      ,(0.090930,0.065699,-0.053657,-0.096140)
//      ,(0.014112,0.098936,0.042017,-0.075099)
//      ,(-0.075680,0.041212,0.099061,0.014988)
//      ,(-0.095892,-0.054402,0.065029,0.091295)
//    )
//    val T2 = Matrix(
//      (0.084147,  -0.075680,   0.065699,  -0.054402,   0.042017,  -0.028790),
//      (0.090930,  -0.095892,   0.098936,  -0.099999,   0.099061,  -0.096140),
//      (0.014112,  -0.027942,   0.041212,  -0.053657,   0.065029,  -0.075099)
//
//    )
//    val X = Matrix(
//      (0.084147,  -0.027942,  -0.099999),
//      (0.090930,   0.065699,  -0.053657),
//      (0.014112,   0.098936,   0.042017),
//      (-0.075680,   0.041212,   0.099061),
//      (-0.095892,  -0.054402,   0.065029)
//    )
//
//    val y = Vector(2, 3, 1, 2, 3)
//

    val T1 = random(5,4, 0.12)
    val T2 = random(3,6, 0.12)
    val X = random(5, 3, 1)
    val y = Vector(2, 3, 1, 2, 3)
    val thetas = T1.flatten :: T2.flatten

    val j = new J(X, Ex4.numbersToBitmaps(y, 3), List((5,4), (3,6)))(lambda = 0)
    val grad = j.gradient(thetas)
    val num_grad = numericGradient(j.cost _, thetas)
    norm2(num_grad - grad)/norm2(num_grad + grad) should be < 1e-9
  }


}