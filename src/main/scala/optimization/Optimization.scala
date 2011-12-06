package optimization

import matrix._

import org.apache.commons.math.analysis.{MultivariateVectorialFunction, MultivariateRealFunction, DifferentiableMultivariateRealFunction}
import org.apache.commons.math.optimization.general.{ConjugateGradientFormula, NonLinearConjugateGradientOptimizer, GaussNewtonOptimizer}
import org.apache.commons.math.optimization._


trait CostFunction{
  def cost(thetas:RowVector) : Double
  def gradient(thetas:RowVector) : RowVector
}


object Optimization{

  def gradient_descent(initial_thetas: RowVector, J: CostFunction, MAX_ITERATIONS: Int = 400, COST_GOAL: Double = 0.5)={
    var thetas = initial_thetas
    var iteration = 0
    var cost = J.cost(thetas)
    while ( cost > COST_GOAL && iteration < MAX_ITERATIONS){
      thetas = thetas - J.gradient(thetas)
      cost = J.cost(thetas)
      iteration+=1
      printf("[%d] Cost=%f\n", iteration, cost)
    }
    thetas
  }

  class ApacheCostFunction(J:CostFunction) extends DifferentiableMultivariateRealFunction(){
    def partialDerivative(k: Int) = new MultivariateRealFunction {
//    def value(theta: Array[Double]) : Double = gradient.value(theta)(k)
      def value(theta: Array[Double]) : Double = throw new Error
    }

    def gradient() = new MultivariateVectorialFunction {
      def value(theta: Array[Double]) = J.gradient(RowVector(theta)).row(0)
    }

    def value(theta: Array[Double]) = J.cost(RowVector(theta))
  }

  def polak_ribiere(thetas: RowVector, J: CostFunction, max_iterations: Int)={


    val costFunction = new ApacheCostFunction(J)
    val opt = new NonLinearConjugateGradientOptimizer(ConjugateGradientFormula.POLAK_RIBIERE)

    opt.setConvergenceChecker(new RealConvergenceChecker{
      def converged(iteration: Int, previous: RealPointValuePair, current: RealPointValuePair) = {
        printf("[%d] Diff=%.4f, Cost=%.2f\n", iteration, previous.getValue- current.getValue, current.getValue)
        iteration == max_iterations || (math.abs(previous.getValue - current.getValue) < 0.005)
      }
    })
    RowVector(opt.optimize(costFunction, GoalType.MINIMIZE , thetas.row(0)).getPoint)
  }

}













