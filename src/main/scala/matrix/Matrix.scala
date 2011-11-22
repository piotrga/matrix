package matrix

import scala.Array._
import scala.math._

abstract class MatrixLike(val items : Array[Array[Double]]){

  def row(i: Int): Array[Double] = items(i)
  def sum : RowVector = rows match {
    case 1 => RowVector(items(0).reduce(_+_))
    case _ => RowVector(items.transpose.map(col => col.reduce(_+_)))
  }

  def apply(row:Int, col:Int) = if (row >= rows || col >= cols) throw new Exception("Index out of bound (%d,%d) matrix size is %dx%d".format(row,col, rows, cols)) else items(row)(col)
  def *(scalar:Double) =  apply(_ * scalar)
  def +(scalar:Double) =  apply(_ + scalar)
  def -(scalar:Double) =  apply(_ - scalar)
  def /(scalar:Double) =  apply(_ / scalar)
  def /:(scalar:Double) = apply(scalar / _)
  def *:(scalar:Double) = this * scalar
  def +:(scalar:Double) = this + scalar
  def -:(scalar:Double) = apply(scalar - _)
  def @^(scalar:Double) = apply(pow (_, scalar))

  def rows = items.length
  def cols = if (items.length == 0) 0 else items(0).length
  def ::(scalar : Double) : Matrix = new Matrix(items.map(scalar +: _))
  def ::(m : MatrixLike) : Matrix = new Matrix(m.items.zip(items).map{case (r1, r2) => r1 ++ r2})
  def T = instance(items.transpose)

  def apply(f : Double => Double)  = instance(items.map(_.map(f(_))))

  def instance(items: Array[Array[Double]]) : MatrixLike

  def +(m : MatrixLike) = combine(m, _+_)
  def -(m : MatrixLike) = combine(m, _-_)
  def @*(m : MatrixLike) = combine(m, _*_)
  def @/(m : MatrixLike) = combine(m, _/_)

  private def combine(v1:Array[Double], v2:Array[Double])(fun:(Double,Double) => Double ) : Array[Double] = v1.zip(v2).map({case(x,y)=>fun(x,y)})

  def combine(m:MatrixLike, fun:(Double,Double) => Double): MatrixLike   = {
    if (m.rows != rows || m.cols != cols) throw new Exception("Not the same matrix sizes %dx%d != %dx%d".format(rows,cols, m.rows,m.cols ))
    val rows_combined = items.zip(m.items)
    instance(rows_combined.map{case (row1, row2) => combine(row1,row2)(fun)})
  }


  def multiply(m: MatrixLike)(implicit multiplyMatrix : (Array[Array[Double]], Array[Array[Double]]) => Array[Array[Double]] = Matrix.scalaMultiplyMatrix ) : MatrixLike = {
    if (cols != m.rows) throw new Exception("Can not multipy matrix %dx%d by %dx%d".format(rows, cols, m.rows, m.cols))
    new Matrix(multiplyMatrix(items, m.items))
  }



  def *(m : MatrixLike)(implicit multiplyMatrix : (Array[Array[Double]], Array[Array[Double]]) => Array[Array[Double]] = Matrix.scalaMultiplyMatrix ) = multiply(m)(multiplyMatrix)
  def *(m : RowVector) = multiply(m)
  def *(m : Vector) : Vector = new Vector(multiply(m).items)
  def dropFirstColumn = instance(items.transpose.drop(1).transpose)
  def toScalar : Double = if (rows == 1 &&  cols == 1) this(0,0) else throw new Exception("Matrix is not a scalar!")

  override def toString = mkString

  protected def magicFormat(value:Double) :String = {
    (try{
      val res = BigDecimal.valueOf(value).bigDecimal.stripTrailingZeros().toEngineeringString
      if (res.length() > 8) "%8f".format(value) else res
    } catch {
      case e : NumberFormatException => "%8f".format(value)
    }).take(9).reverse.padTo(9, ' ').reverse
  }

  def mkString = "Matrix(%dx%d):\n%s\n".format(rows,cols, items.map(_.map(magicFormat).mkString(" ")).mkString("\n"))

  override def equals(p1: Any) = p1 match{
    case m : MatrixLike => items.deep.equals(m.items.deep)
    case _ => false
  }

}

class Matrix(items:Array[Array[Double]])extends MatrixLike(items){
  override def instance(new_items:Array[Array[Double]]) : Matrix = new Matrix(new_items)

  def inverse = {
    import org.apache.commons.math.linear.RealMatrixImpl
    new Matrix(new RealMatrixImpl(items).inverse().getData)
  }
}

object Vector{
  def apply(data:Double*) = new Vector(Array(data.toArray).transpose)
  def apply(data:Array[Double]) = new Matrix(Array(data).transpose)
}

object RowVector{
  def apply(data:Double*) = new RowVector(Array(data.toArray))
  def apply(data:Array[Double]) = new RowVector(Array(data))
}

class Vector(items:Array[Array[Double]]) extends MatrixLike(items){
  if (this.cols != 1) throw new Exception("Vector can only have one column but it has %d!" format(this.cols) )
  override def T : RowVector = new RowVector(items.transpose)

  override def instance(items: Array[Array[Double]]) : Vector = new Vector(items)
}

class RowVector(items:Array[Array[Double]]) extends MatrixLike(items){
  if (this.rows != 1) throw new Exception("RowVector can only have one row but it has %d!" format(this.rows) )

  override def *(m : MatrixLike)(implicit multiplyMatrix : (Array[Array[Double]], Array[Array[Double]]) => Array[Array[Double]] = Matrix.scalaMultiplyMatrix ) : RowVector = new RowVector(super.*(m)(multiplyMatrix).items)
  //  override def *(m : Vector) : Double = super.*(m).toScalar
  override def T : Vector = new Vector(items.transpose)

  def diag = {
    val res = Array.fill(cols, cols)(0.0)
    for (i <- 0 to cols-1) res(i)(i) = this(0, i)
    new Matrix(res)
  }

  override def instance(items: Array[Array[Double]]) : RowVector = new RowVector(items)
}



object Matrix{
  import math.{min,  max}

  /*
 This function might look complicated, but I had to sacrifice readability for efficiency here.
  */
  def scalaMultiplyMatrix(m1: Array[Array[Double]], m2: Array[Array[Double]]) :  Array[Array[Double]] = {
    val res =  Array.ofDim[Double](m1.length, m2(0).length)
    val M1_COLS = m1(0).length
    val M1_ROWS = m1.length
    val M2_COLS = m2(0).length

    val N = M1_ROWS * M1_COLS * M2_COLS
    val PARTITIONS : Int = max(1, min(M1_ROWS, min(N / 20000, 256)))
    val PARTITION_ROWS : Int = M1_ROWS/PARTITIONS

// THIS IS SO SLOW!
//    @inline def singleThreadedMultiplication(row_range: Range) {
//      for (row_index <- row_range;
//           col_index <- 0 until M2_COLS) {
//        var sum = 0.0
//        for (i <- 0 until M1_COLS) {
//          sum += m1(row_index)(i) * m2(i)(col_index)
//        }
//
//        res(row_index)(col_index) = sum
//      }
//    }


    @inline def singleThreadedMultiplicationFAST(row_range: Range) = {
      var col, i  = 0
      var sum = 0.0
      var row = row_range.start
      val M1_COLS_MINUS = M1_COLS - 8

      // while statements are much faster than for statements
      while(row < row_range.end){ col = 0
        while(col < M2_COLS){ i = 0; sum = 0
          while(i < M1_COLS_MINUS){
            // This has to be superfast and unrolling this loop cuts down on execution
            sum += m1(row)(i) * m2(i)(col); i+=1
            sum += m1(row)(i) * m2(i)(col); i+=1
            sum += m1(row)(i) * m2(i)(col); i+=1
            sum += m1(row)(i) * m2(i)(col); i+=1
            sum += m1(row)(i) * m2(i)(col); i+=1
            sum += m1(row)(i) * m2(i)(col); i+=1
          };
          while(i<M1_COLS){
            sum += m1(row)(i) * m2(i)(col); i+=1
          }

          res(row)(col) = sum
          col += 1

        }; row += 1
      }
      res
    }

    @inline def parallelMultiplication = {
      import akka.dispatch.Future
      val futures = for (i <- 0 until PARTITIONS) yield Future {
        singleThreadedMultiplicationFAST((i * PARTITION_ROWS) until (min(M1_ROWS, (i + 1) * PARTITION_ROWS)))
      }
      futures.foreach(_.await)
      res
    }

    if (PARTITIONS > 1)
      parallelMultiplication
    else
      singleThreadedMultiplicationFAST(0 until M1_ROWS)

  }

  def apacheMultiplyMatrix(m1:Array[Array[Double]], m2:Array[Array[Double]]): Array[Array[Double]] = {
    import org.apache.commons.math.linear.RealMatrixImpl
    new RealMatrixImpl(m1).multiply(new RealMatrixImpl(m2)).getData
  }

  def apply(t2 : Product*) = new Matrix(t2.toArray.map(t => t.productIterator.map(_ match {
    case d: {def toDouble: Double} => d.toDouble
    case d: Double => d
    case x => throw new Exception("Element "+x+ "is not convertible to double")
  }).toArray))

  def apply(row: Double, rest:Double*) = new Matrix(Array((row+:rest).toArray))

  implicit def arrayToVector(items:Array[Double]): Vector = Vector(items)

  implicit def scalarToMatrix(scalar:Double): Matrix = new Matrix(Array(Array(scalar)))
  implicit def intArrayToMatrix(items:Array[Int]): Matrix = Vector(items.map(_.toDouble))
  implicit def seqIntToSeqDouble(s:Seq[Int]):Seq[Double] = s.map(_.toDouble)
  implicit def intToDouble(i:Int):Double = i.toDouble

}

