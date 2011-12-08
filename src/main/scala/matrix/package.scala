import java.util.UUID

package object matrix {
  def sqrt[T <: MatrixLike](m : T): T#Repr =  m.apply(math.sqrt _)
  def log[T <: MatrixLike](m : T) :T#Repr =  m.apply(math.log _)
  def √[T <: MatrixLike](m : T) =  sqrt(m)
  def sum(m : Matrix) : RowVector  =  m.sum
  def ∑(m : Matrix) =  sum(m)
  def sum(m : RowVector) : Double =  m.sum
  def ∑(m : RowVector) =  sum(m)
  def sum(m : Vector) : Double =  m.sum
  def ∑(m : Vector) =  sum(m)


  def std(m : Matrix) : RowVector = sqrt(sum(m @^ 2) / m.row_count)
  def diag(m : RowVector) : Matrix = m.diag
  def ones(rows:Int, cols:Int) =  new Matrix(Array.fill(rows, cols)(1.0))
  def zeros(rows:Int, cols:Int) =  new Matrix(Array.fill(rows, cols)(0.0))
  def norm2(m: RowVector) = math.sqrt(sum(m@^2))

  def sigmoid[T <: MatrixLike](m : T) : T#Repr = m(x => 1/(1+math.exp(-x)))
  def round[T<:MatrixLike](m:T) : T#Repr = m(math.round(_).toDouble)


  def random(rows: Int, columns: Int, range : Double): Matrix = {
    new Matrix(Array.fill(rows, columns) {
      (scala.math.random * 2 - 1) * range
    })
  }

  class MatrixOps(d:Double){
    def *[T<:MatrixLike](m:T) : T#Repr = m * d
    def /[T<:MatrixLike](m:T) : T#Repr = m(d/_)
    def +[T<:MatrixLike](m:T) : T#Repr = m + d
    def -[T<:MatrixLike](m:T) : T#Repr = m(d - _)
  }


  implicit def intLikeToMatrixOps(d: Int) : MatrixOps = new MatrixOps(d.toDouble)
  implicit def intLikeToMatrixOps(d: Double) : MatrixOps = new MatrixOps(d.toDouble)
  implicit def intLikeToMatrixOps(d: BigDecimal) : MatrixOps = new MatrixOps(d.toDouble)
  implicit def intLikeToMatrixOps(d: BigInt) : MatrixOps = new MatrixOps(d.toDouble)


  //  trait Instance[A]{
  //    def apply(items : Array[Array[Double]]) : A
  //  }
  //
  //  implicit object MatrixInstance extends Instance[Matrix]{
  //    def apply(items : Array[Array[Double]])  = new Matrix(items)
  //  }
  //
  //  implicit object RowVectorInstance extends Instance[RowVector]{
  //    def apply(items : Array[Array[Double]])  = new RowVector(items)
  //  }
  //
  //  implicit object VectorInstance extends Instance[Vector]{
  //    def apply(items : Array[Array[Double]])  = new Vector(items)
  //  }





  implicit def VectorToMatrix(v:Vector) : Matrix = new Matrix(v.items)
  implicit def MatrixToRowVector(M : Matrix) : RowVector = new RowVector(M.items)
  implicit def MatrixToVector(M : Matrix) : Vector = new Vector(M.items)
  //  implicit def MatrixLikeToMatrix(M : MatrixLike) : Matrix = new Matrix(M.items)

  @inline def printTime[T](label:String)(block : => T) = {
    val start = System.nanoTime()
    try{
      block
    }finally {
      val duration = System.nanoTime() - start
      printf("%s took %f ms\n", label, duration/1000000.0)
    }
  }

  def benchmark(iterations:Int)( block : => Unit):Double = {
    val start = System.nanoTime()
    var i = 0
    while(i<iterations) {block;i+=1}
    val res = ((System.nanoTime() - start) /1000000).toDouble/ iterations
    res
  }

}
