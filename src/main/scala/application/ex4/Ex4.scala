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

class J(X:Matrix, y:Vector) extends CostFunction{
  lazy val Y = {
    val mapping = for(i <- 0 to 9) yield {
      val res = Array.fill(10)(0.0)
      res(i) = 1
      res
    }

    new Matrix(for(row<-y.items; item = row(0) % 10) yield {mapping(item.toInt)})
  }
  def sigmoid(x:Double) = 1/(1+math.exp(-x))
  def sigmoid(X:Matrix) = X(x => 1/(1+math.exp(-x)))

  def cost(thetas: RowVector) = {
    val List(t1, t2) = thetas.reshape((401, 25), (26,10))
    val A1 = sigmoid((1::X) * t1)

    val H  = sigmoid((1::A1) * t2)
//    println("A1:"+A1)
//    println("H:"+H)

    val costs = (-Y @* log(H)) - ((1-Y) @* log(1 - H));

    ∑(∑(costs))/X.rows
  }

  def gradient(theta: RowVector) = theta
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
