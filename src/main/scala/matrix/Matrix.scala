package matrix

import scala.Array._
import scala.math._
import io.Source
import annotation.tailrec
import java.io.{File, PrintWriter}

trait Multiply[M1, M2, DST]{
  def apply(m1:M1,  m2:M2) : DST
}

object Multiply{
  implicit object MultiplyRowVectorByVector extends Multiply[RowVector, Vector, Double]{
    def apply(m1: RowVector, m2: Vector) : Double = (m1.multiply(m2)).apply(0).apply(0)
  }

  implicit object MultiplyRowVectorByMatrix extends Multiply[RowVector, Matrix, RowVector]{
    def apply(m1: RowVector, m2: Matrix) : RowVector = new RowVector(m1.multiply(m2))
  }

  implicit object MultiplyMatrixByVector extends Multiply[Matrix, Vector, Vector]{
    def apply(m1: Matrix, m2: Vector) = new Vector(m1.multiply(m2))
  }

  implicit object MultiplyMatrixByRowVector extends Multiply[Matrix, RowVector, Matrix]{
    def apply(m1: Matrix, m2: RowVector) = new Matrix(m1.multiply(m2))
  }

  implicit object MultiplyMatrixByMatrix extends Multiply[Matrix, Matrix, Matrix]{
    def apply(m1: Matrix, m2: Matrix) = new Matrix(m1.multiply(m2))
  }

}

abstract class MatrixLike(val items:Array[Array[Double]])(implicit operations : MatrixOperations = ActorBasedMatrixOperations){
  type Repr <: MatrixLike
  def flatten : RowVector = RowVector(items.transpose.flatten)

  def rows = items.map(RowVector(_:_*))
  lazy val row_count = items.length
  lazy val cols = if (items.length == 0) 0 else items(0).length
  lazy val fast_apply = if (row_count* cols > 20000) operations.apply _ else operations.small_apply _


  def row(i: Int): Array[Double] = items(i)

  def apply(row:Int, col:Int) = if (row >= row_count || col >= cols) throw new Exception("Index out of bound (%d,%d) matrix size is %dx%d".format(row,col, row_count, cols)) else items(row)(col)
  def *[A, B](m: A)(implicit multiply : Multiply[Repr, A,  B])  = multiply(this.asInstanceOf[Repr], m)

  def unary_-() = apply(-_)
  def *(scalar:Double) =  apply(_ * scalar)
  def +(scalar:Double) =  apply(_ + scalar)
  def -(scalar:Double) =  apply(_ - scalar)
  def /(scalar:Double) =  apply(_ / scalar)
  def /:(scalar:Double) = apply(scalar / _)
  def *:(scalar:Double) = this * scalar
  def +:(scalar:Double) = this + scalar
  def -:(scalar:Double) = apply(scalar - _)
  def @^(scalar:Double) = apply(pow (_, scalar))




  @inline def apply(f : Double => Double) = instance(fast_apply(items, f) )

  @inline def instance(items: Array[Array[Double]]) : Repr

  def +(m : MatrixLike) = combine(m, _+_)
  def -(m : MatrixLike) = combine(m, _-_)
  def @*(m : MatrixLike) = combine(m, _*_)
  def @/(m : MatrixLike) = combine(m, _/_)


  @inline private def combine(v1:Array[Double], v2:Array[Double])(fun:(Double,Double) => Double ) : Array[Double] = v1.zip(v2).par.map({case(x,y)=>fun(x,y)}).toArray


  @inline def combine(m:MatrixLike, fun:(Double,Double) => Double) : Repr = {
    if (m.row_count != row_count || m.cols != cols) throw new Exception("Not the same matrix sizes %dx%d != %dx%d".format(row_count,cols, m.row_count,m.cols ))
    val rows_combined = items.zip(m.items)
    instance(rows_combined.par.map{case (row1, row2) => combine(row1,row2)(fun)}.toArray)
  }

  def multiply(m: MatrixLike) : Array[Array[Double]] = {
    if (cols != m.row_count) throw new Exception("Can not multipy matrix %dx%d by %dx%d".format(row_count, cols, m.row_count, m.cols))
    operations.multiply(items, m.items)
  }

  def dropFirstColumn = instance(items.par.map(_.drop(1)).toArray)
  def toScalar : Double = if (row_count == 1 &&  cols == 1) this(0,0) else throw new Exception("Matrix is not a scalar!")

  override def toString = mkString

  protected def magicFormat(value:Double) :String = {
    (try{
      val res = BigDecimal.valueOf(value).bigDecimal.stripTrailingZeros().toEngineeringString
      if (res.length() > 8) "%8f".format(value) else res
    } catch {
      case e : NumberFormatException => "%8f".format(value)
    }).take(9).reverse.padTo(9, ' ').reverse
  }

  def mkString = "Matrix(%dx%d):\n%s\n".format(row_count,cols, items.map(_.map(magicFormat).mkString(" ")).mkString("\n"))

  override def equals(p1: Any) = p1 match{
    case m : MatrixLike => items.deep.equals(m.items.deep)
    case _ => false
  }

  def saveToFile(filePath:String) = {
    val p = new PrintWriter(new File(filePath))
    p.write(items.map(_.mkString(" ")).mkString("\n"))
    p.close()
    this
  }
}


class Matrix(items:Array[Array[Double]]) extends MatrixLike(items){
  type Repr = Matrix

  def sum : RowVector = row_count match {
    case 1 => RowVector(items(0).reduce(_+_))
    case _ => RowVector(items.transpose.par.map(col => col.reduce(_+_)).toArray)
  }

  def inverse = {
    import org.apache.commons.math.linear.RealMatrixImpl
    new Matrix(new RealMatrixImpl(items).inverse().getData)
  }

  def T = new Matrix(items.transpose)
  def ᵀ = this.T
  def ::(scalar : Double) : Matrix = new Matrix(items.par.map(scalar +: _).toArray)
  def ::(m : MatrixLike) : Matrix = new Matrix(m.items.par.zip(items).map{case (r1, r2) => r1 ++ r2}.toArray)

  def instance(items: Array[Array[Double]]) = new Matrix(items)
}

class Vector(items:Array[Array[Double]]) extends MatrixLike(items){
  type Repr = Vector
  if (this.cols != 1) throw new Exception("Vector can only have one column but it has %d!" format(this.cols) )
  def T : RowVector = new RowVector(items.transpose)
  def ᵀ = this.T
  def sum : Double = items.transpose.apply(0).reduce(_+_)


  def ::(scalar : Double) : Matrix = scalar :: new Matrix(items)
  def ::(m : MatrixLike) : Matrix = m :: new Matrix(items)

  def instance(items: Array[Array[Double]]) = new Vector(items)
}

class RowVector(items:Array[Array[Double]]) extends MatrixLike(items){
  type Repr = RowVector
  if (this.row_count != 1) throw new Exception("RowVector can only have one row but it has %d!" format(this.row_count) )

  def T : Vector = new Vector(items.transpose)
  def ᵀ = this.T

  def diag = {
    val res = Array.fill(cols, cols)(0.0)
    for (i <- 0 to cols-1) res(i)(i) = this(0, i)
    new Matrix(res)
  }

  //  def *(v:Vector) : Double = multiply(v).apply(0).apply(0)

  def sum : Double = items(0).sum

  def instance(items: Array[Array[Double]]) = new RowVector(items)
  def ::(scalar : Double) : RowVector = new RowVector(items.map(scalar +: _))
  def ::(r : RowVector) : RowVector = new RowVector(Array(r.items(0) ++ items(0)))
  def reshape(dimensions : (Int, Int)*) : List[Matrix] = {
    @tailrec def r(dimensions : List[(Int, Int)], data:Seq[Double], accumulator: List[Matrix]) : List[Matrix] = dimensions match{
      case (rows,cols) :: dimTail => {
        if(data.length < rows*cols) throw new Exception("Not enough elements. Dimensions left: "+ dimensions)
        val res = Array.ofDim[Double](rows,cols)
        val (current, dataTail) = data.splitAt(rows*cols)
        for(row<- 0 until rows; col <- 0 until cols) res(row)(col) = current(row+col*rows)
        r (dimTail, dataTail, new Matrix(res)::accumulator)
      }
      case Nil => {
        if (data.length > 0 ) throw new Exception("There are %d elements left! Not enough dimensions.".format(data.length))
        accumulator.reverse
      }
    }
    r(dimensions.toList, items(0), Nil)
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

object Matrix{

  /*
 This function might look complicated, but I had to sacrifice readability for efficiency here.
  */

  def fromFile(filename:String) = {
    def readMatrix(filePath: String): Array[Array[Double]] = {
      Source.fromFile(filePath).getLines().filter(!_.startsWith("#")).filter(!_.trim().isEmpty).map(_.trim().split(" ").map(_.toDouble)).toArray
    }

    new Matrix(readMatrix(filename))
  }

  def apply(t2 : Product*) = new Matrix(t2.toArray.map(t => t.productIterator.map(_ match {
    case d: {def toDouble: Double} => d.toDouble
    case d: Double => d
    case x => throw new Exception("Element "+x+ "is not convertible to double")
  }).toArray))

  def apply(row: Double, rest:Double*) = new Matrix(Array((row+:rest).toArray))

  implicit def arrayToVector(items:Array[Double]): Vector = Vector(items:_*)
  implicit def arrayToRowVector(items:Array[Double]): RowVector = RowVector(items:_*)

  //  implicit def scalarToMatrix(scalar:Double): Matrix = new Matrix(Array(Array(scalar)))
  implicit def intArrayToMatrixVector(items:Array[Int]) = Vector(items.map(_.toDouble):_*)
  implicit def intArrayToVector(items:Array[Int]) = Vector(items.map(_.toDouble):_*)
  implicit def seqIntToSeqDouble(s:Seq[Int]):Seq[Double] = s.map(_.toDouble)

}

