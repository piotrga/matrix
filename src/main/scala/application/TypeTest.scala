package application

trait AbstractCurrency{
  type Currency <: AbstractCurrency
  val value : Double

  def make(d:Double) : Currency
  def +(x: AbstractCurrency) = make(x.value + value)
}

class USD(_value: Double) extends AbstractCurrency {
  type Currency = USD
  val value = _value

  def make(d: Double) = new USD(d)
}

object X{
  def plus[T <: AbstractCurrency](c1: T,  c2:T) : T#Currency = c1 + c2
  val x : USD = plus(new USD(100),new USD(200))
}