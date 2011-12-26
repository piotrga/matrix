package sandbox

object CurrencyWithAbstractType {
  trait CurrencyLike{
    type Currency <: CurrencyLike
    def value : Double

    def make(d:Double) : Currency
    def +(x: Currency) = make(x.value + value)
  }

  type AbstractCurrency = X forSome {type X <: CurrencyLike{ type Currency = X}}

  class USD(val value: Double) extends CurrencyLike {
    type Currency = USD
    def make(d: Double) : USD = new USD(d)
  }

  class EUR(val value: Double) extends CurrencyLike {
    type Currency = EUR
    def make(d: Double) = new EUR(d)
  }

//  def plus[T <: AbstractCurrency](c1: T, c2: T) = c1 + c2

//  val x : USD = plus(new USD(100),new USD(200))
  new EUR(100) + new EUR(50)

//  plus(new USD(100))(new EUR(200))
  //new EUR(100) + new USD(50)

}

object CurrencyWithParametrizedType{

  trait CurrencyLike[Repr <: CurrencyLike[Repr]]{
    val value : Double

    def make(d:Double) : Repr
    def +(x: Repr) = make(x.value + value)
  }

  class USD(val value: Double) extends CurrencyLike[USD] { def make(d: Double) = new USD(d) }
  class EUR(val value: Double) extends CurrencyLike[EUR] { def make(d: Double) = new EUR(d) }

  def plus[T <: CurrencyLike[T]](c1: T,  c2:T) : T = c1 + c2
}
