package sandbox

import testing.Benchmark
import matrix.MyMatrixOperations

object  MatrixMultiplicationEvolution extends App{

  @inline def singleThreadedMultiplication1(m1:Seq[Array[Double]], m2:Array[Array[Double]] ) ={
    val res =  Array.ofDim[Double](m1.length, m2(0).length)
    var col, i  = 0
    var row = 0

    // while statements are much faster than for statements
    while(row < m1.length){ col = 0
      while(col < m2(0).length){ i = 0
        while(i < m1(0).length){
          res(row)(col) += m1(row)(i) * m2(i)(col)
          i+=1
        }
        col += 1
      }
      row += 1
    }
    res
  }

  @inline def singleThreadedMultiplication2(m1:Seq[Array[Double]], m2:Array[Array[Double]] ) ={
    val res =  Array.fill(m1.length, m2(0).length)(0.0)
    var sum = 0.0
    for(row <- 0 until m1.length;
        col <- 0 until m2(0).length){
      sum = 0
      for (i   <- 0 until m1(0).length){
        sum += m1(row)(i) * m2(i)(col)
      }
      res(row)(col) = sum

    }

    res
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

  def multiThreadedIdiomatic(m1:Seq[Array[Double]], m2:Array[Array[Double]] ) ={
    val res =  Array.fill(m1.length, m2(0).length)(0.0)
    for(row <- (0 until m1.length).par;
        col <- (0 until m2(0).length).par;
        i   <- 0 until m1(0).length){
      res(row)(col) += m1(row)(i) * m2(i)(col)
    }
    res
  }

  val matrix = Array.fill(300, 300)(1.0)


  val resParIdio = new Benchmark{
    def run() {
      multiThreadedIdiomatic(matrix, matrix)
    }
  }.runBenchmark(100).reduce(_+_)/100
  printf("resParIdio =%d ms\n", resParIdio)
  System.gc();

  val resPar = new Benchmark{
    def run() {
      MyMatrixOperations.multiply(matrix, matrix)
    }
  }.runBenchmark(100).reduce(_+_)/100
  printf("resPar =%d ms\n", resPar)
  System.gc();

  val res2 = new Benchmark{
    def run() {
      singleThreadedMultiplication2(matrix, matrix)
    }
  }.runBenchmark(100).reduce(_+_)/100
  printf("res2 =%d ms\n", res2)
  System.gc();

  val res1 = new Benchmark{
    def run() {
      singleThreadedMultiplication1(matrix, matrix)
    }
  }.runBenchmark(100).reduce(_+_)/100
  printf("res1 =%d ms\n", res1)
  System.gc();

  val resFast = new Benchmark{
    def run() {
      singleThreadedMultiplicationFAST(matrix, matrix)
    }
  }.runBenchmark(100).reduce(_+_)/100
  printf("resFast =%d ms\n", resFast)
  System.gc();



}