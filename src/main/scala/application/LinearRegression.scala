package application

import org.apache.commons.math.analysis.{MultivariateVectorialFunction, MultivariateRealFunction, DifferentiableMultivariateRealFunction}
import matrix._
import matrix.Matrix._
import org.apache.commons.math.optimization.general.{ConjugateGradientFormula, NonLinearConjugateGradientOptimizer}
import org.apache.commons.math.optimization.GoalType


trait CostFunction{
  def cost(X:Matrix,  y:Vector, theta:Vector) : Double
  def gradient(X:Matrix,  y:Vector, theta:Vector) : RowVector
}


class ApacheCostFunction(X1:Matrix, y:Vector, J:CostFunction) extends DifferentiableMultivariateRealFunction(){
  val X = 1::X1

  def partialDerivative(k: Int) = new MultivariateRealFunction {
    def value(theta: Array[Double]) : Double = gradient.value(theta)(k)
  }


  def gradient() = new MultivariateVectorialFunction {
    def value(theta: Array[Double]) = J.gradient(X, y, theta).row(0)
  }

  def value(theta: Array[Double]) = J.cost(X, y, theta)
}


class LinearRegression(J:CostFunction){

  val opt = new NonLinearConjugateGradientOptimizer(ConjugateGradientFormula.FLETCHER_REEVES)

  def normalize(X:Matrix):(Matrix, RowVector, RowVector) = {
    val mean : RowVector = ∑(X) / X.rows
    val Mean = ones(X.rows, 1) * mean
    val Diff = X - Mean
    val std = matrix.std(Diff)
    val Std = diag(1/:std)
    (Diff * Std, mean, std)
  }

  def normalize(X:Matrix, mean:RowVector, std:RowVector) : Matrix = {
    val Mean = ones(X.rows,1) * mean
    val Std = diag(1/:std)
    (X - Mean) * Std
  }

  def findSolution(features:Matrix, y:Vector) : (Matrix) => Matrix = {
    val (xmulti, mean, std ) = normalize(features)

    val theta = Vector(opt.optimize( new ApacheCostFunction(xmulti, y, J), GoalType.MINIMIZE, Array.fill(features.cols+1)(0.0)).getPoint)

    return (X:Matrix) => (1 :: normalize(X, mean, std)) * theta
  }

}


