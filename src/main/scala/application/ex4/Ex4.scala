package application.ex4

import matrix._
import annotation.tailrec

import optimization.{CostFunction, Optimization}


object Ex4 extends App{
  /**
   * Converts number label to zero-one RowVector ie. 3 -> [0 0 1 0 0], 4 -> [0 0 0 1 0]
   */
  def numbersToBitmaps(y:Vector, number_of_labels:Int) : Matrix = {
    val mapping = diag(ones(1, number_of_labels))

    new Matrix(for(row<-y.items; i = row(0).toInt) yield mapping.row(i-1))
  }
  

  val network_dimentions = List((25, 401), (10, 26))
  val X = Matrix.fromFile("data/X.txt")
  val y = Matrix.fromFile("data/Y.txt")
  val Y = numbersToBitmaps(y, 10)


//  val thetas = Matrix.fromFile("data/theta.txt").T
//  val thetas = Matrix.fromFile("data/out.txt").T
  def thetas_size(network_dimentions: List[(Int, Int)]): Int = network_dimentions.foldLeft(0)((a, dim) => a + dim._1 * dim._2)

  val initial_thetas = random(1, thetas_size(network_dimentions), 0.12 )
  val j = new J(X, Y, network_dimentions)(lambda = 0.01)

  val calculated_thetas = Optimization.polak_ribiere(initial_thetas, j, max_iterations = 40)
  calculated_thetas saveToFile "data/out.txt"

  val solution = new NeuronNetwork(calculated_thetas.reshape(network_dimentions:_*)).solve(X)
  val diff = Y.rows.zip(solution.rows).filter({ x => !x._1.equals(x._2)})

  printf("Efficiency %.2f%%\n", 100 - diff.length *100.0 / Y.row_count)

}

class NeuronNetwork(val theta_layers: List[Matrix]){
  private[this] def feed_forward(X:Matrix) = NeuronNetwork.feed_forward(X, theta_layers)
  def solve(X:Matrix) = feed_forward(X).head.apply(math.round(_))
}



object NeuronNetwork{
  def feed_forward(X:Matrix, theta_layers: List[Matrix]) = {
    @tailrec def feed(network_layer:Matrix, thetas : List[Matrix], accu: List[Matrix]) : List[Matrix] = thetas match {
      case theta :: xs => {
        val next_layer = sigmoid(network_layer * theta.T)
        feed (1::next_layer, xs, next_layer :: accu)
      }
      case Nil => accu
    }

    feed(1 :: X, theta_layers, X :: Nil)
  }
}

class J(X:Matrix, Y:Matrix, layersDim:List[(Int, Int)])(implicit val lambda : Double = 1) extends CostFunction{
  lazy val X1 = 1 :: X
  lazy val m = X.row_count


  private[this] def feed_forward(thetas: RowVector) : List[Matrix] = NeuronNetwork.feed_forward(X,  thetas.reshape(layersDim:_*))

  def cost(thetas: RowVector) = {
    val theta_layers = thetas.reshape(layersDim:_*)

    val H  = NeuronNetwork.feed_forward(X, theta_layers).head

    val costs = (-Y @* log(H)) - ((1-Y) @* log(1 - H))
    val reg = (lambda/(2*m)) * theta_layers.par.map(t => ∑(∑(t.dropFirstColumn @^ 2)) ).sum;
    
    val cost = (∑(∑(costs))/m + reg)
    cost
  }

  def gradient(thetas: RowVector) = {
    val List(t1, t2) = thetas.reshape(layersDim:_*)
    val layers = feed_forward(thetas)

    val H = layers(0)
    val A2 = 1 :: layers(1)
    val A1 = X1

//    Backprop
    val Delta3 = H - Y
    val reg2 = 0 :: ((lambda/m) * t2.dropFirstColumn)
    val T2_grad = Delta3.T * A2 / m + reg2

    val Delta2 = (Delta3 * t2) @* (A2 @* (1-A2))
    val reg1 = 0 :: ((lambda/m) * t1.dropFirstColumn)
    val T1_grad  = Delta2.dropFirstColumn.T * A1/m + reg1

    T1_grad.flatten :: T2_grad.flatten
  }
}