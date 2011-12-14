package application


trait X{
  val y : Int
  val z = y*y
}

class Abc extends { val y = 7 } with X{
   def printZ = { println(z) }
}

object Z extends App{
  new Abc().printZ
  if ( true) { print("asdq") }

}


