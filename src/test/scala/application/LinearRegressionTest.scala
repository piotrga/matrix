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
      val diff =   X * theta - y
      val value = (diff.T * diff).toScalar / (2.0 * theta.rows)
      val reg = sum(theta.T.dropFirstColumn).toScalar * lambda/(2*theta.rows)
//      println("Value=" + value)
      value + reg

    }

    def gradient(X: Matrix, y: Vector, theta: Vector) : RowVector = {
      val diff = X * theta  - y
      val grad : RowVector = ((diff.T * X) / (alpha/theta.rows))
      val theta2 = (0 :: theta.T.dropFirstColumn)
      val reg = theta2 * lambda / theta.rows
      grad + reg
    }

  }


  "Linear regression" should "find theta" in {
    val features = toFeatures(X)
//    println("Features="+features)
    val solution = new LinearRegression(J).findSolution(features, y)
    val range = Vector((0.0 to 50.0).by(5).toArray)
    val example = toFeatures(range)

    val estimation = solution(example)
    println(range :: estimation)

    estimation(3,0) should be ( -0.2354682798481262 plusOrMinus 0.000001)

  }

}