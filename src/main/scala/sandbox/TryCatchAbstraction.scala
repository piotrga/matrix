package sandbox

import java.nio.channels.ClosedChannelException

object TryCatchAbstraction {

  connected {
    doSomethingDangerous()
  } otherwise {
    println("Error! Got disconnected.")
  }

  def doSomethingDangerous() = {/*...*/}

  def connected(body: => Unit): Result =
    try {
      body; Ok
    } catch {
      case e: ClosedChannelException => Failed
    }

  trait Result { def otherwise(onError: => Unit): Unit}
  case object Ok extends Result { def otherwise(onError: => Unit) = () /* do nothing */}
  case object Failed extends Result { def otherwise(onError: => Unit) = onError}

}