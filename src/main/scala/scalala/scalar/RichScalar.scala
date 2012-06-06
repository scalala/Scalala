package scalala.scalar

class RichScalar(value: Double) {
  def +(c: Complex): Complex = Complex(value, 0) + c;
  def -(c: Complex): Complex = Complex(value, 0) - c;
  def *(c: Complex): Complex = Complex(value, 0) * c;
  def /(c: Complex): Complex = Complex(value, 0) / c;
}
