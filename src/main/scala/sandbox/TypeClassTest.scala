package sandbox

import java.io.{File, InputStream}


object TypeClassTest {
  trait FileLike[T] {
    def name(file : T) : String
    def isDirectory(file : T) : Boolean
    def children(directory : T) : Seq[T]
    def child(parent : T, name : String) : T
    def mkdirs(file : T) : Unit
    def content(file : T) : InputStream
    def writeContent(file : T, otherContent : InputStream) : Unit
  }

  implicit val fileFileLike = new FileLike[File] {
    def name(file: File) = null

    def isDirectory(file: File) = false

    def children(directory: File) = null

    def child(parent: File, name: String) = null

    def mkdirs(file: File) {}

    def content(file: File) = null

    def writeContent(file: File, otherContent: InputStream) {}
  }


  def sum[T: AddTypeClass](list: List[T]) = {
    val typeClass = implicitly[AddTypeClass[T]]
    list.reduce(typeClass.add(_,_))
  }



  def sum[T](list: Seq[T])(implicit typeClass : AddTypeClass[T]) = {
    list.reduce(typeClass.add(_,_))
  }

  trait AddTypeClass[T]{
    def add(a:T,  b:T) : T
  }

  implicit val ints = new AddTypeClass[Int]{
    def add(a: Int, b: Int) = a+b
  }

  implicit val strings = new AddTypeClass[String]{
    def add(a: String, b: String) = a+b
  }

  implicit def lists[T] = new AddTypeClass[List[T]]{
    def add(a: List[T], b: List[T]) = a:::b
  }

  val l : List[Int] = sum(List(List(1,2), List(3,4)))



  def xxx[F : FileLike](a: F) = {
    val aa = implicitly[FileLike[F]]
    println(aa.isDirectory(a))
  }

  case class FunnyPair[A,B](a:A, b:B)
  val x = FunnyPair
  x(2,"9")

  type ::[A,B] = FunnyPair[A, B]
  def add(x: Int :: Int) = x.a + x.b
}