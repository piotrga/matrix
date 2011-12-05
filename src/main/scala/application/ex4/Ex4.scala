package application.ex4

import matrix._
import java.io.{PrintWriter, File}
import annotation.tailrec
import collection.parallel.mutable
import org.apache.commons.math.analysis.solvers._

//import matrix.{MatrixToRowVector}
import application.LinearRegression
import org.apache.commons.math.analysis.{MultivariateVectorialFunction, MultivariateRealFunction, DifferentiableMultivariateRealFunction}
import org.apache.commons.math.optimization.general.{ConjugateGradientFormula, NonLinearConjugateGradientOptimizer, GaussNewtonOptimizer}
import org.apache.commons.math.optimization._

import matrix.Matrix._


object Ex4 extends App{

  def numbersToBitmaps(y:Vector, labels:Int) : Matrix = {
    val mapping = diag(ones(1, labels))

    new Matrix(for(row<-y.items; i = row(0).toInt) yield mapping.row(i-1))
  }
  
  def gradient_descent(initial_thetas: RowVector, J: CostFunction)={
    val m = initial_thetas.cols
    var thetas = initial_thetas
    var i = 0
    var cost = J.cost(thetas)
    while ( cost > 0.5 && i < 400){
      thetas = thetas - (J.gradient(thetas))
      cost = J.cost(thetas)
      i+=1
      printf("[%d] Cost=%f\n", i, cost)
    }
    thetas
  }

  def optimize(thetas: RowVector, J: CostFunction, max_iterations: Int)={
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


  val network_dimentions = List((25, 401), (10, 26))
  val X = Matrix.fromFile("data/X.txt")
  val y = Matrix.fromFile("data/Y.txt")
  val Y = numbersToBitmaps(y, 10)


//  val thetas = Matrix.fromFile("data/theta.txt").T
//  val thetas = Matrix.fromFile("data/out.txt").T
  def thetas_size(network_dimentions: List[(Int, Int)]): Int = network_dimentions.foldLeft(0)((a, dim) => a + dim._1 * dim._2)

  val initial_thetas = random(1, thetas_size(network_dimentions), 0.12 )
  val j = new J(X,Y, network_dimentions)(lambda = 0.01)

  val calculated_thetas = optimize(initial_thetas, j, max_iterations = 40)
  calculated_thetas saveToFile "data/out.txt"

  val solution = (j.feed_forward(calculated_thetas).last).apply(math.round(_))
  val diff = Y.rows.zip(solution.rows).filter({ x => !x._1.equals(x._2)})

  printf("Efficiency %.2f%%\n", 100 - diff.length *100.0 / Y.row_count)
//  printf("Diff = %s", diff.foldLeft("")((s,x) => s+x._1+"\n"+x._2 ))



}
  


class J(X:Matrix, Y:Matrix, layersDim:List[(Int, Int)])(implicit val lambda : Double = 1) extends CostFunction{
  lazy val X1 = 1 :: X
  lazy val m = X.row_count


  def feed_forward(thetas: RowVector) : List[Matrix] = feed_forward(thetas.reshape(layersDim:_*))
  def feed_forward(theta_layers: List[Matrix]) = {
    @tailrec def feed(network_layer:Matrix, thetas : List[Matrix], accu: List[Matrix]) : List[Matrix] = thetas match {
      case theta :: xs => {
        val next_layer = sigmoid(network_layer * theta.T)
        feed (1::next_layer, xs, next_layer :: accu)
      }
      case Nil => accu
    }

    feed(X1, theta_layers, X :: Nil)
  }

  def cost(thetas: RowVector) = {
    val theta_layers = thetas.reshape(layersDim:_*)

    val H  = feed_forward(theta_layers).head

    val costs = (-Y @* log(H)) - ((1-Y) @* log(1 - H))
    val reg = (lambda/(2*m)) * theta_layers.map(t => ∑(∑(t.dropFirstColumn @^ 2)) ).sum;
    
    val cost = (∑(∑(costs))/m + reg)
    cost
  }

  def gradient(thetas: RowVector) = {
    val List(t1, t2) = thetas.reshape(layersDim:_*)
    val layers = feed_forward(thetas)

    val H = layers(0)
    val A2 = 1 :: layers(1)
    val A1 = X1

    val Delta3 = H - Y
    val reg2 = 0 :: ((lambda/m) * t2.dropFirstColumn)
    val T2_grad = Delta3.T * A2 / m + reg2

    val Delta2 = (Delta3 * t2) @* (A2 @* (1-A2))
    val reg1 = 0 :: ((lambda/m) * t1.dropFirstColumn)
    val T1_grad  = Delta2.dropFirstColumn.T * A1/m + reg1

    T1_grad.flatten :: T2_grad.flatten
  }
}


trait CostFunction{
  def cost(thetas:RowVector) : Double
  def gradient(thetas:RowVector) : RowVector
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