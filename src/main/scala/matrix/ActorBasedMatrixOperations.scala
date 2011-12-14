package matrix

import utils._
import akka.routing._
import akka.actor.Actor._
import akka.dispatch.{MessageDispatcher, Future}
import akka.actor.Actor


object ActorBasedMatrixOperations extends MatrixOperations{

  import math.{min,  max, round}

  override def apply(items: Array[Array[Double]], f: (Double) => Double) = MyMatrixOperations.apply(items, f)

  case class MultiplyMatrix(m1:Seq[Array[Double]], m2:Array[Array[Double]])
  case class Apply(m1:Array[Array[Double]], f :Double=>Double)

  //

  class Multiplier(dispatcher: MessageDispatcher) extends akka.actor.Actor{
    self.setDispatcher(dispatcher)
    protected def receive = {
      case MultiplyMatrix(m1, m2) => self reply singleThreadedMultiplicationFAST (m1,m2)
      case Apply(m1, f) => self reply MyMatrixOperations.apply (m1, f)
    }
  }


  @inline def singleThreadedMultiplicationFAST(m1:Seq[Array[Double]], m2:Array[Array[Double]] ) ={
    val res =  Array.ofDim[Double](m1.length, m2(0).length)
    val M1_COLS = m1(0).length
    val M1_ROWS = m1.length
    val M2_COLS = m2(0).length

    var col, i  = 0
    var sum = 0.0
    var row = 0

    // while statements are much faster than for statements
    while(row < M1_ROWS){ col = 0
      while(col < M2_COLS){ i = 0; sum = 0
        while(i<M1_COLS){
          sum += m1(row)(i) * m2(i)(col)
          i+=1
        }

        res(row)(col) = sum
        col += 1

      }; row += 1
    }
    res
  }

  lazy val multiplier = {
    import akka.util.duration._
    val daemonic_dispatcher = akka.dispatch.Dispatchers.newExecutorBasedEventDrivenDispatcher("multipliers")
      .setCorePoolSize(8)
      .setMaxPoolSize(32)
      .setKeepAliveTime(1 second)
      .setDaemonic(true)
      .build

    val multipliers = List.fill(32)(actorOf(new Multiplier(daemonic_dispatcher)).start())
    val balancer = actorOf(new Actor with LoadBalancer {
      self.setDispatcher(daemonic_dispatcher)
      val seq = new CyclicIterator(multipliers)
    }).start()


    Runtime.getRuntime.addShutdownHook( balancer+:multipliers foreach(_.stop) )
    balancer
  }

  override def multiply(m1: Array[Array[Double]], m2: Array[Array[Double]]) :  Array[Array[Double]] = {
    val M1_COLS = m1(0).length
    val M1_ROWS = m1.length
    val M2_COLS = m2(0).length

    val N = M1_ROWS * M1_COLS * M2_COLS
    val PARTITIONS : Int = max(1, min(M1_ROWS, min(N / 20000, 256)))
    val PARTITION_ROWS : Int = round(M1_ROWS.toDouble/PARTITIONS).toInt

    Future.traverse((0 until M1_ROWS).grouped(PARTITION_ROWS).toTraversable){ partition_range =>
      val slice = m1.slice(partition_range.head, partition_range.last+1)
      (multiplier ? MultiplyMatrix(slice, m2)).mapTo[Array[Array[Double]]]
    }.get.reduce(_++_)
  }

}






