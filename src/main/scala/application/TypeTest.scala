package application


object CurrencyWithAbstractType{

  trait AbstractCurrency{
    type Currency <: AbstractCurrency
    def value : Double

    def make(d:Double) : Currency
    def +(x: AbstractCurrency/*doesnt compile if I change it to Currency*/) = make(x.value + value)
  }

  class USD(val value: Double) extends AbstractCurrency {
    type Currency = USD
    def make(d: Double) = new USD(d)
  }
  class EUR(val value: Double) extends AbstractCurrency {
    type Currency = EUR
    def make(d: Double) = new EUR(d)
  }

  def plus[T <: AbstractCurrency](c1: T,  c2:T) : T#Currency = c1 + c2
  val x : USD = plus(new USD(100),new USD(200))

  new EUR(100) + new USD(50) //compiles which is wrong
}

object CurrencyWithParametrizedType{

  trait AbstractCurrency[Currency <: AbstractCurrency[Currency]]{
    val value : Double

    def make(d:Double) : Currency
    def +(x: Currency) = make(x.value + value)
  }

  class USD(val value: Double) extends AbstractCurrency[USD] {
    def make(d: Double) = new USD(d)
  }

  class EUR(val value: Double) extends AbstractCurrency[EUR] {
    def make(d: Double) = new EUR(d)
  }

  def plus[T <: AbstractCurrency[T]](c1: T,  c2:T) : T = c1 + c2
  val x : USD = plus(new USD(100),new USD(200))
//  new EUR(100) + new USD(50) // doesnt compile whis is expected

}
