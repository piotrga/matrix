package matrix

import sun.reflect.generics.reflectiveObjects.NotImplementedException


trait MatrixOperations{
  def apply(items: Array[Array[Double]], f: (Double) => Double): Array[Array[Double]] = small_apply(items, f)
  def small_apply(items: Array[Array[Double]], f: (Double) => Double): Array[Array[Double]] = items.map(_.map(f(_)))

  def multiply(m1: Array[Array[Double]], m2: Array[Array[Double]]) :  Array[Array[Double]] = throw new NotImplementedException
}

object MyMatrixOperations extends MatrixOperations{


  //  override def apply(items: Array[Array[Double]], f: (Double) => Double) = items.par.map(_.map(f(_))).toArray
  override def apply(items: Array[Array[Double]], f: (Double) => Double) = {
    val rows = items.length
    val cols = items(0).length

    val res = Array.ofDim[Double](rows, cols)
    @inline def distribute_by_rows = {
      (0 until rows).par.foreach { row =>
        var col = 0
        while (col < cols) {
          res(row)(col) = f(items(row)(col))
          col += 1
        }
      }
      res
    }

    @inline def distribute_by_cols ={
      (0 until cols).par.foreach { col =>
        var row = 0
        while (row < rows) {
          res(row)(col) = f(items(row)(col))
          row += 1
        }
      }
      res
    }

    if (rows  < cols)
      distribute_by_rows
    else
      distribute_by_cols

  }


  override def multiply(m1: Array[Array[Double]], m2: Array[Array[Double]]) :  Array[Array[Double]] = {
    val res =  Array.ofDim[Double](m1.length, m2(0).length)
    val M1_COLS = m1(0).length
    val M1_ROWS = m1.length
    val M2_COLS = m2(0).length

    @inline def singleThreadedMultiplicationFAST(start_row:Int,  end_row:Int) {
      var col, i  = 0
      var sum = 0.0
      var row = start_row

      // while statements are much faster than for statements
      while(row < end_row){ col = 0
        while(col < M2_COLS){ i = 0; sum = 0
          while(i<M1_COLS){
            sum += m1(row)(i) * m2(i)(col)
            i+=1
          }

          res(row)(col) = sum
          col += 1

        }; row += 1
      }
    }

    (0 until M1_ROWS).par.foreach{ i =>
      singleThreadedMultiplicationFAST(i, i+1)
    }

    res

  }

}



object ApacheMatrixOperations extends MatrixOperations{
  override def multiply(m1:Array[Array[Double]], m2:Array[Array[Double]]): Array[Array[Double]] = {
    import org.apache.commons.math.linear.RealMatrixImpl
    new RealMatrixImpl(m1).multiply(new RealMatrixImpl(m2)).getData
  }
}
