package object matrix {
  def sqrt[T <: MatrixLike[_]](m : MatrixLike[T]) : T =  m.apply(math.sqrt _)
  def √[T <: MatrixLike[_]](m : MatrixLike[T]) =  sqrt(m)
  def sum(m : Matrix) : RowVector  =  m.sum
  def ∑(m : Matrix) =  sum(m)
  def sum(m : RowVector) : Double =  m.sum
  def ∑(m : RowVector) =  sum(m)
  def sum(m : Vector) : Double =  m.sum
  def ∑(m : Vector) =  sum(m)

  def std(m : Matrix) : RowVector = sqrt(sum(m @^ 2) / m.rows)
  def diag(m : RowVector) : Matrix = m.diag
  def ones(rows:Int, cols:Int) =  new Matrix(Array.fill(rows, cols)(1.0))


  def random(rows: Int, columns: Int, range : Double): Matrix = {
    new Matrix(Array.fill(rows, columns) {
      (scala.math.random * 2 - 1) * range
    })
  }

  class MatrixOps(d:Double){
    def *(m:MatrixLike[_]) = m * d
    def /(m: MatrixLike[_]) = m(d/_)
    def +(m:MatrixLike[_]) = m + d
    def -(m:MatrixLike[_]) = m(d - _)
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
    val start = System.currentTimeMillis()
    try{
      block
    }finally {
      val duration = System.currentTimeMillis() - start
      printf("%s took %d ms\n", label, duration)
    }
  }
}
