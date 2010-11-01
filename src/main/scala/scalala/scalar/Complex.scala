/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package scalar;

/**
 * Immutable complex numbex number representation backed by doubles
 * for the real and imaginary parts.
 *
 * @author dramage
 */
case class Complex(real : Double, imag : Double) {
  override def toString = real + " + " + imag + "i";

  def +(that : Complex) =
    Complex(this.real + that.real, this.imag + that.imag);

  def +(that : Int) =
    Complex(this.real + that, this.imag);

  def +(that : Long) =
    Complex(this.real + that, this.imag);

  def +(that : Float) =
    Complex(this.real + that, this.imag);

  def +(that : Double) =
    Complex(this.real + that, this.imag);

  def -(that : Complex) =
    Complex(this.real - that.real, this.imag - that.imag);

  def -(that : Int) =
    Complex(this.real - that, this.imag);

  def -(that : Long) =
    Complex(this.real - that, this.imag);

  def -(that : Float) =
    Complex(this.real - that, this.imag);

  def -(that : Double) =
    Complex(this.real - that, this.imag);

  def *(that : Complex) =
    Complex(this.real * that.real - this.imag * that.imag,
            this.real * that.imag + this.imag * that.real);

  def *(that : Int) =
    Complex(this.real * that, this.imag * that);

  def *(that : Long) =
    Complex(this.real * that, this.imag * that);

  def *(that : Float) =
    Complex(this.real * that, this.imag * that);

  def *(that : Double) =
    Complex(this.real * that, this.imag * that);

  def /(that : Complex) = {
    val denom = that.real * that.real + that.imag * that.imag;
    Complex((this.real * that.real + this.imag * that.imag) / denom,
            (this.imag * that.real - this.real * that.imag) / denom);
  }

  def /(that : Int) =
    Complex(this.real / that, this.imag / that);

  def /(that : Long) =
    Complex(this.real / that, this.imag / that);

  def /(that : Float) =
    Complex(this.real / that, this.imag / that);

  def /(that : Double) =
    Complex(this.real / that, this.imag / that);

  def unary_- =
    Complex(-real, -imag);

  def abs =
    math.sqrt(real*real + imag*imag);

  def conjugate =
    Complex(real, -imag);

  override def equals(that : Any) = that match {
    case that : Complex => this.real == that.real && this.imag == that.imag;
    case real : Int => this.real == real && this.imag == 0;
    case real : Short => this.real == real && this.imag == 0;
    case real : Long => this.real == real && this.imag == 0;
    case real : Float => this.real == real && this.imag == 0;
    case real : Double => this.real == real && this.imag == 0;
    case _ => false;
  }
}

object Complex {
  /**
   * Ordering for complex numbers: orders lexicographically first on
   * the real, then on the imaginary part of the number.
   */
  implicit object ordering extends Ordering[Complex] {
    override def compare(a : Complex, b : Complex) = {
      if (a.real < b.real) -1
      else if (a.real > b.real) 1
      else if (a.imag < b.imag) -1
      else if (a.imag > b.imag) 1
      else 0;
    }
  }

  implicit object scalar extends Scalar[Complex] {
    def zero = Complex(0,0);

    def one = Complex(1,0);

    def ==(a : Complex, b : Complex) = a == b;

    def !=(a : Complex, b : Complex) = a != b;

    def >(a : Complex, b : Complex) =
      (a.real > b.real || (a.real == b.real && a.imag > b.imag));
    
    def >=(a : Complex, b : Complex) =
      (a.real >= b.real || (a.real == b.real && a.imag >= b.imag));

    def <(a : Complex, b : Complex) =
      (a.real < b.real || (a.real == b.real && a.imag < b.imag));

    def <=(a : Complex, b : Complex) =
      (a.real <= b.real || (a.real == b.real && a.imag <= b.imag));

    def +(a : Complex, b : Complex) = a + b;

    def -(a : Complex, b : Complex) = a - b;

    def *(a : Complex, b : Complex) = a * b;
    
    def /(a : Complex, b : Complex) = a / b;

    def norm(a : Complex) = a.abs;

    def toDouble(a : Complex) =
      throw new UnsupportedOperationException("Cannot automatically convert complext numbers to doubles");
    
    def isNaN(a : Complex) =
      a.real.isNaN || a.imag.isNaN;
    
    val manifest = implicitly[ClassManifest[Complex]];
  }
}
