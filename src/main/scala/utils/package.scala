package object utils{
  implicit def unitToThread(block : => Unit) : Thread = new Thread(){
    override def run() { block }
  }
}