package application

import matrix._
import matrix.Matrix.arrayToVector
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec


class LinearRegressionTest extends FlatSpec with ShouldMatchers{
  val X = Vector(  8,  15.0,  20,  25,   30,    35,    40,   45)
  val y = Vector( -2,  -0.3, 0.4, 0.1,  0.0, -0.25, -0.75, -1.0)

  def toFeatures(X : MatrixLike) = {
    (X@^2) :: (X@^3) :: (X@^4) :: (X @^6) :: (X@^1) :: (X@^7) :: X(math.log) :: X(math.cos)
  }


  val J = new CostFunction {
    val lambda = 1.0
    val alpha = 1.0

    def cost(X: Matrix, y: Vector, theta: Vector) : Double = {
      val m = X.rows
      val diff =   X * theta - y
      val value = (diff.T * diff).toScalar / (2.0 * m)
      val reg = sum(theta.T.dropFirstColumn).toScalar * lambda / (2 * m)
      value + reg

    }

    def gradient(X: Matrix, y: Vector, theta: Vector) : RowVector = {
      val m = X.rows
      val diff = X * theta  - y
      val grad : RowVector = ((diff.T * X) * (alpha / m))
      val regularization = (0 :: theta.T.dropFirstColumn) * lambda / m
      grad + regularization
    }

  }


  "Linear regression" should "find theta" in {
    val features = toFeatures(X)
//    println("Features="+features)

    val solution = new LinearRegression(J).findSolution(features, y)
    val range = Vector((0.0 to 45.0).by(2.5).toArray)
    val example = toFeatures(range)

    val estimation = solution(example)
    println(range :: estimation)

    estimation(3,0) should be (  -1.3389483845601906 plusOrMinus 0.000001)

  }

}