
package matrix

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.math.Pi
import matrix.Matrix._
import io.Source
import java.io.{PrintWriter, FileOutputStream, File}


class MatrixTest extends FlatSpec with ShouldMatchers {
  val X = Matrix(
    (3, 4),
    (5, 6))
  val Y =  Matrix(
    (1, 2),
    (1, 2))

  "Matrix" should "be multiplied by matrix" in   {
   val  PROD : Matrix = X * Y

    PROD.should( be(Matrix(
      ( 7, 14),
      (11, 22))))

    (X * Y).should( not be Matrix(
      ( 5, 8),
      (20, 9)))

  }


  it should "add to matrix" in{

    (X + Y).should( be(Matrix(
      ( 4, 6),
      ( 6, 8))))

  }

  it should "substract from matrix" in{

    (X - Y).should( be(Matrix(
      ( 2, 2),
      ( 4, 4))))

  }

  it should "substract scalar" in{

    (Y - 1).should( be(Matrix(
      ( 0, 1),
      ( 0, 1))))
  }

  it should "be substracted from scalar" in{

    (1 - Y).should( be(Matrix(
      ( 0, -1),
      ( 0, -1))))
  }

  it should "add scalar" in{

    (Y + 1).should( be(Matrix(
      (2, 3),
      (2, 3))))

    (1 + Y).should( be(Y+1))
  }

  it should "be multiplied by scalar: Y * 2" in{


    val Expected = Matrix(
      (2, 4),
      (2, 4))
    (Y * 2).should( be(Expected))
    (2 * Y).should( be(Expected))
  }

  it should "be divided by scalar: Y / 2" in{

    (Y / 0.5).should( be(Matrix(
      ( 2, 4),
      ( 2, 4))))

  }

  it should "divide scalar: 2 /: Y" in{

    val Expected = Matrix(
      (1, 0.5),
      (1, 0.5))

    (1 /: Y).should( be(Expected))
    (1 / Y).should( be(Expected))
    (1.0 / Y).should( be(Expected))
    (BigInt(1) / Y).should( be(Expected))

  }

  it should "multiply by another matrix fast" in{
    var start = System.currentTimeMillis()

    val M = random(500,400, 1)
    val M1 = random(400, 250, 1)
    val M2 = random(250, 10, 1)

    println("Random init: "+ (System.currentTimeMillis() - start))
    (M * M1 * M2)

    start = System.currentTimeMillis()
    (M * M1 * M2)
//    println("Multiplication: "+ (System.currentTimeMillis() - start))
    (System.currentTimeMillis() - start) should be < (200L )
  }



  it should "prepend column" in{
    (1 :: X).should(be(Matrix(
      (1, 3, 4),
      (1, 5, 6)
    )))
  }

  it should "drop first column" in{
    (1::1::X).dropFirstColumn.should(be(1::X))
  }

  it should "be transposable" in {
    (Matrix(
    (3, 4),
    (5, 6)).T).should(be(Matrix(
    (3, 5),
    (4, 6))))
  }

  it should "convert to scalar if has only one item" in{
    def square(a:Double) = a*a
    square(Vector(1.1).toScalar).should(be(1.21 plusOrMinus 0.01))
  }

  it should  "print well" in {
    Matrix((1.1, -Pi),(Pi, -4)).mkString should be("""Matrix(2x2):
      1.1 -3.141593
 3.141593        -4
""")
  }

  it should  "convert Array[Double] to column vector" in {
//    Vector(0,0,0) + Array(1.0,2,3) should be (Vector(1,2,3))
  }

  it should  "sum columns" in {
    Matrix((1,2),(3,4)).sum should be (Vector(4,6).T)
  }

  it should  "sum one row to scalar" in {
    Matrix(1, 2).sum should be (Vector(3.0))
  }

  it should  "extract rows" in {
    Matrix((1, 2), (3,4)).row(0) should be (Array(1,2))
  }

  it should  "append another matrix: X :: Y" in {
    Vector(1, 2) :: Vector(3,4) should be (Matrix((1,3),(2,4)))
    Matrix((1, 2),(3,4)) :: Vector(3,5) should be (Matrix( (1,2,3), (3,4,5)))

    val X = Matrix((1, 2),
                   (3, 4))
    val Y = Matrix((5, 6),
                   (7, 8))

    X :: Y should be (Matrix((1, 2, 5, 6),
                             (3, 4, 7, 8))
    )
  }

  it should "multiply corresponding elemets" in {
    val X = Matrix((1,2),(4,5))
    X @* X should be (Matrix((1, 4), (16, 25)))
  }

  it should "divide corresponding elemets" in {
    val X = Matrix((1,2),(4,5))
    X @/ X should be (Matrix((1, 1), (1, 1)))
  }

  it should  "calculate sum of columns" in {
    matrix.sum( Matrix((1,2), (3,4), (5,6))) should be(RowVector(9, 12))
  }

  it should  "calculate sum of one row matrix" in {
    matrix.sum(RowVector(1,2,3)) should be(6)
  }

  it should  "calculate sqrt" in {
    sqrt(Matrix((4,9,16))) should be(Matrix((2, 3, 4)))
  }

  it should  "calculate std" in {
    val X = Matrix((math.sqrt(7), 3), (2, 3), (4, 3))
    std(X) should  be (Matrix((3, 3)))
  }

  it should "implicitly convert to Vector if has one column" in {
    val X = Matrix(1,2,3).T
    val x : Vector = X
    x should be(Vector(1,2,3))
  }

  it should "blow up if converted to vector and has more columns" in {
    val X = Matrix((1,2),(3,4))
    val thrown = evaluating  {val x : Vector = X} should produce [Exception]
    thrown.getMessage should be ("Vector can only have one column but it has 2!")
  }

  it should "implicitly convert to RowVector if has one row" in {
    val X = Matrix(1,2,3)
    val x : RowVector = X
    x should be(RowVector(1,2,3))
  }

  it should "blow up if converted to RowVector and has more rows" in {
    val X = Matrix((1,2),(3,4))
    val thrown = evaluating  {val x : RowVector = X} should produce [Exception]
    thrown.getMessage should be ("RowVector can only have one row but it has 2!")
  }

  it should "read from file" in  {
    val tmp = new PrintWriter("tmp.txt")
    tmp.write("""# Created by Octave 3.2.4, Fri Nov 11 01:45:57 2011 GMT <piotrga@Tygrysek.local>
# name: y
# type: matrix
# rows: 5000
# columns: 1
 10 0.1234

 10 20
 10 1.234e+10
 10 4
""")
    tmp.close()
    try{
      Matrix.fromFile("tmp.txt") should be (Matrix((10, 0.1234), (10, 20), (10, 1.234e+10), (10, 4)))

    }finally{
      new File("tmp.txt").delete()
    }

  }


  it should "flatten to row vector" in {
    (X :: Y).flatten should be (RowVector(3,4,1,2,5,6,1,2))
  }

  it should "reshape flat data" in{
    X.flatten.reshape((2,2)) should be(List(X))
    (X.flatten :: Y.flatten).reshape((2,2), (2,2)) should be(List(X, Y))
    RowVector(1,2,3,4,5,6).reshape((2,3)) should be (List(Matrix((1,2,3),(4,5,6))))
  }


//  "MyMatrixOperations" should "multiply correctly big matricies" in {
//    val M = random(5003, 400, 1)
//    val M1 = random(400,25, 1)
//    ApacheMatrixOperations.multiply(M.items, M1.items) should be(MyMatrixOperations.multiply(M.items, M1.items))
//  }


}






