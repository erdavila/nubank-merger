package nubank

case class Amount(centavos: Int) {
  def unary_- : Amount = Amount(-centavos)
  def + (that: Amount): Amount = Amount(this.centavos + that.centavos)
}
