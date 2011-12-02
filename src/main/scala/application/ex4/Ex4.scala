package application.ex4

import matrix._
import application.LinearRegression
import org.apache.commons.math.analysis.{MultivariateVectorialFunction, MultivariateRealFunction, DifferentiableMultivariateRealFunction}
import matrix.Matrix._


object Ex4 extends App{

  def optimize(thetas: RowVector, J: CostFunction)={
    Nil
  }

  val X = Matrix.fromFile("data/X.txt")
  val y = Matrix.fromFile("data/Y.txt")
  val thetas = Matrix.fromFile("data/theta.txt").T

//  val Theta1 = random(401, 25, 0.00012)
//  val Theta2 = random(26, 10, 0.000012)
//
//  val thetas = Theta1.flatten :: Theta2.flatten

//  println(new J(X,y).Y)
  println(new J(X,y, List((401,25), (26,10))).cost(thetas))
//  optimize(thetas, new J(X,y))



}

class J(X:Matrix, Y:Matrix, layersDim:List[(Int, Int)])(implicit val lambda : Double = 1) extends CostFunction{
  lazy val X1 = 1 :: X
  lazy val m = X.rows

  def sigmoid(x:Double) : Double = 1.0/(1.0 + math.exp(-x))
  def sigmoid(X:Matrix) = X(x => 1/(1+math.exp(-x)))
  def round(X:Matrix) = X(math.round(_).toDouble)

  def feed_forward(t1:Matrix, t2:Matrix) : List[Matrix] = {
    val A1 = sigmoid(X1 * t1.T)
    val A2 = sigmoid((1 :: A1) * t2.T)
    List(A1, A2)
  }

  def cost(thetas: RowVector) = {
    val List(t1, t2) = thetas.reshape(layersDim:_*)

    val H = (feed_forward(t1, t2).last)

    val costs = (-Y @* log(H)) - ((1-Y) @* log(1 - H))
    val reg = (lambda/(2*m)) * ( ∑(∑(t1.dropFirstColumn @^ 2)) + ∑(∑(t2.dropFirstColumn @^ 2)) );

    (∑(∑(costs))/m + reg)
  }

  def gradient(thetas: RowVector) = {
    val List(t1, t2) = thetas.reshape(layersDim:_*)
    val layers = feed_forward(t1, t2)
    val A2 = 1:: layers(0)

    val H = layers.last
    val Delta3 = H - Y
    val Delta2 = (Delta3 * t2) @* (A2 @* (1-A2))
    val reg1 = 0 :: ((lambda/m) * t1.dropFirstColumn)
    val reg2 = 0 :: ((lambda/m) * t2.dropFirstColumn)
    val T1_grad  = Delta2.dropFirstColumn.T * X1/m + reg1
    val T2_grad = Delta3.T * A2 / m + reg2

    T1_grad.flatten :: T2_grad.flatten
  }
}


trait CostFunction{
  def cost(thetas:RowVector) : Double
  def gradient(thetas:RowVector) : RowVector
}


class ApacheCostFunction(J:CostFunction) extends DifferentiableMultivariateRealFunction(){
  def partialDerivative(k: Int) = new MultivariateRealFunction {
    def value(theta: Array[Double]) : Double = gradient.value(theta)(k)
  }

  def gradient() = new MultivariateVectorialFunction {
    def value(theta: Array[Double]) = J.gradient(RowVector(theta)).row(0)
  }

  def value(theta: Array[Double]) = J.cost(RowVector(theta))
}