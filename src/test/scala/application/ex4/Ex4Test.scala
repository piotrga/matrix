package application.ex4

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import matrix._


class Ex4Test extends FlatSpec with ShouldMatchers{

  lazy val X = Matrix.fromFile("data/X.txt")
  lazy val y = Matrix.fromFile("data/Y.txt")
  lazy val thetas = Matrix.fromFile("data/theta.txt").T

  "CostFunction" should "convert numbers to 01 vectors" in {
    val costFun = new J(new Matrix(Array.ofDim(1,1)), Vector(10,1,2,3,4,5,6,7,8,9))
    costFun.Y should be (Matrix(
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
    new J(X,y)(lambda = 0).cost(thetas) should be (0.287629 plusOrMinus 0.0000005)
  }

  it should "calculate correct gradient without regularization" in {
    new J(X,y)(lambda = 0).gradient(thetas) should be (RowVector(0))
  }

  it should "calculate correct cost with regularization" in {
    new J(X,y)(lambda = 1).cost(thetas) should be (0.383770 plusOrMinus 0.0000005)

  }

}