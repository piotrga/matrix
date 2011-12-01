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
  println(new J(X,y).cost(thetas))
//  optimize(thetas, new J(X,y))



}

class J(X:Matrix, y:Vector)(implicit val lambda : Double = 1) extends CostFunction{
  lazy val m = X.rows
  lazy val Y = {
    val mapping = diag(ones(1, 10))

    new Matrix(for(row<-y.items; i = row(0).toInt) yield mapping.row(i-1))
  }
  def sigmoid(x:Double) : Double = 1.0/(1.0 + math.exp(-x))
  def sigmoid(X:Matrix) = X(x => 1/(1+math.exp(-x)))
  def round(X:Matrix) = X(math.round(_).toDouble)

  def feed_forward(t1:Matrix, t2:Matrix) : List[Matrix] = {
    val A1 = sigmoid((1 :: X) * t1)
    val A2 = sigmoid((1 :: A1) * t2)
    List(A1, A2)
  }

  def cost(thetas: RowVector) = {
    val List(t1, t2) = thetas.reshape((401, 25), (26, 10))

    val H = feed_forward(t1, t2).last

    val costs = (-Y @* log(H)) - ((1-Y) @* log(1 - H))
    val reg = (lambda/(2*m)) * ( ∑(∑(t1.T.dropFirstColumn @^ 2)) + ∑(∑(t2.T.dropFirstColumn @^ 2)) );

    ∑(∑(costs))/m + reg
  }

  def gradient(thetas: RowVector) = {
    val List(t1, t2) = thetas.reshape((401, 25), (26, 10))
    val layers = feed_forward(t1, t2)
    val A2 = 1:: layers(0)
    val Delta3 = layers.last - Y
    val Delta2 = (Delta3 * t2.T) @* (A2 @* (1-A2))
    val reg1 = 0 :: ((lambda/m) * t1.T.dropFirstColumn)
    val reg2 = 0 :: ((lambda/m) * t2.T.dropFirstColumn)
    val T1_grad  = Delta2.dropFirstColumn.T * (1::X)/m + reg1
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
