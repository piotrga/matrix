package object matrix {
  def sqrt(m : MatrixLike) =  m.apply(math.sqrt _)
  def sum(m : MatrixLike) : RowVector =  m.sum

  def std(m : MatrixLike) : RowVector = sqrt(sum(m @^ 2) / m.rows)
  def diag(m : RowVector) : Matrix = m.diag
  def ones(rows:Int, cols:Int) =  new Matrix(Array.fill(rows, cols)(1.0))


  def random(rows: Int, columns: Int, range : Double): Matrix = {
    new Matrix(Array.fill(rows, columns) {
      (scala.math.random * 2 - 1) * range
    })
  }

  class MatrixOps(d:Double){
    def *(m:Matrix) = m * d
    def /(m:Matrix) = m(d/_)
  }


  implicit def intLikeToMatrixOps(d: Int) : MatrixOps = new MatrixOps(d.toDouble)
  implicit def intLikeToMatrixOps(d: Double) : MatrixOps = new MatrixOps(d.toDouble)
  implicit def intLikeToMatrixOps(d: BigDecimal) : MatrixOps = new MatrixOps(d.toDouble)
  implicit def intLikeToMatrixOps(d: BigInt) : MatrixOps = new MatrixOps(d.toDouble)

  implicit def MatrixToRowVector(M : MatrixLike) : RowVector = new RowVector(M.items)
  implicit def MatrixToVector(M : MatrixLike) : Vector = new Vector(M.items)
  implicit def MatrixLikeToMatrix(M : MatrixLike) : Matrix = new Matrix(M.items)

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
