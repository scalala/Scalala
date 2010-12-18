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

import scalala.generic.{CanNeg,CanCast,CanAdd,CanMul,CanSub,CanDiv,CanMod,CanPow};

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

object Complex { outer =>
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

  /** Constant Complex(0,0). */
  val zero = new Complex(0,0);

  /** Constant Complex(1,0). */
  val one = new Complex(1,0);

  /** Constant Complex(NaN, NaN). */
  val nan = new Complex(Double.NaN, Double.NaN);

  /** Constant Complex(0,1). */
  val i = new Complex(0,1);

  //
  // scalar
  //

  implicit object scalar extends Scalar[Complex] {
    def zero = outer.zero;

    def one = outer.one;

    def nan = outer.nan;

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
    
    val defaultArrayValue =
      implicitly[scalala.collection.sparse.DefaultArrayValue[Complex]];
  }

  //
  // neg
  //

  implicit object neg extends CanNeg[Complex,Complex] {
    override def apply(v : Complex) = -v;
  }

  //
  // cast
  //
  
  implicit object castIC extends CanCast[Int,Complex] {
    override def apply(v : Int) = Complex(v,0);
  }

  implicit object castLC extends CanCast[Long,Complex] {
    override def apply(v : Long) = Complex(v,0);
  }

  implicit object castFC extends CanCast[Float,Complex] {
    override def apply(v : Float) = Complex(v,0);
  }

  implicit object castDC extends CanCast[Double,Complex] {
    override def apply(v : Double) = Complex(v,0);
  }

  //
  // add
  //

  implicit object addIC extends CanAdd[Int,Complex,Complex]
  { def apply(a : Int, b : Complex) = a + b; }

  implicit object addLC extends CanAdd[Long,Complex,Complex]
  { def apply(a : Long, b : Complex) = a + b; }

  implicit object addFC extends CanAdd[Float,Complex,Complex]
  { def apply(a : Float, b : Complex) = a + b; }

  implicit object addDC extends CanAdd[Double,Complex,Complex]
  { def apply(a : Double, b : Complex) = a + b; }

  implicit object addCI extends CanAdd[Complex,Int,Complex]
  { def apply(a : Complex, b : Int) = a + b; }

  implicit object addCL extends CanAdd[Complex,Long,Complex]
  { def apply(a : Complex, b : Long) = a + b; }

  implicit object addCF extends CanAdd[Complex,Float,Complex]
  { def apply(a : Complex, b : Float) = a + b; }

  implicit object addCD extends CanAdd[Complex,Double,Complex]
  { def apply(a : Complex, b : Double) = a + b; }

  implicit object addCC extends CanAdd[Complex,Complex,Complex]
  { def apply(a : Complex, b : Complex) = a + b; }

  //
  // sub
  //

  implicit object subIC extends CanSub[Int,Complex,Complex]
  { def apply(a : Int, b : Complex) = a - b; }

  implicit object subLC extends CanSub[Long,Complex,Complex]
  { def apply(a : Long, b : Complex) = a - b; }

  implicit object subFC extends CanSub[Float,Complex,Complex]
  { def apply(a : Float, b : Complex) = a - b; }

  implicit object subDC extends CanSub[Double,Complex,Complex]
  { def apply(a : Double, b : Complex) = a - b; }

  implicit object subCI extends CanSub[Complex,Int,Complex]
  { def apply(a : Complex, b : Int) = a - b; }

  implicit object subCL extends CanSub[Complex,Long,Complex]
  { def apply(a : Complex, b : Long) = a - b; }

  implicit object subCF extends CanSub[Complex,Float,Complex]
  { def apply(a : Complex, b : Float) = a - b; }

  implicit object subCD extends CanSub[Complex,Double,Complex]
  { def apply(a : Complex, b : Double) = a - b; }

  implicit object subCC extends CanSub[Complex,Complex,Complex]
  { def apply(a : Complex, b : Complex) = a - b; }

  //
  // mul
  //

  implicit object mulIC extends CanMul[Int,Complex,Complex]
  { def apply(a : Int, b : Complex) = a * b; }

  implicit object mulLC extends CanMul[Long,Complex,Complex]
  { def apply(a : Long, b : Complex) = a * b; }

  implicit object mulFC extends CanMul[Float,Complex,Complex]
  { def apply(a : Float, b : Complex) = a * b; }

  implicit object mulDC extends CanMul[Double,Complex,Complex]
  { def apply(a : Double, b : Complex) = a * b; }

  implicit object mulCI extends CanMul[Complex,Int,Complex]
  { def apply(a : Complex, b : Int) = a * b; }

  implicit object mulCL extends CanMul[Complex,Long,Complex]
  { def apply(a : Complex, b : Long) = a * b; }

  implicit object mulCF extends CanMul[Complex,Float,Complex]
  { def apply(a : Complex, b : Float) = a * b; }

  implicit object mulCD extends CanMul[Complex,Double,Complex]
  { def apply(a : Complex, b : Double) = a * b; }

  implicit object mulCC extends CanMul[Complex,Complex,Complex]
  { def apply(a : Complex, b : Complex) = a * b; }

  //
  // div
  //

  implicit object divIC extends CanDiv[Int,Complex,Complex]
  { def apply(a : Int, b : Complex) = a / b; }

  implicit object divLC extends CanDiv[Long,Complex,Complex]
  { def apply(a : Long, b : Complex) = a / b; }

  implicit object divFC extends CanDiv[Float,Complex,Complex]
  { def apply(a : Float, b : Complex) = a / b; }

  implicit object divDC extends CanDiv[Double,Complex,Complex]
  { def apply(a : Double, b : Complex) = a / b; }

  implicit object divCI extends CanDiv[Complex,Int,Complex]
  { def apply(a : Complex, b : Int) = a / b; }

  implicit object divCL extends CanDiv[Complex,Long,Complex]
  { def apply(a : Complex, b : Long) = a / b; }

  implicit object divCF extends CanDiv[Complex,Float,Complex]
  { def apply(a : Complex, b : Float) = a / b; }

  implicit object divCD extends CanDiv[Complex,Double,Complex]
  { def apply(a : Complex, b : Double) = a / b; }

  implicit object divCC extends CanDiv[Complex,Complex,Complex]
  { def apply(a : Complex, b : Complex) = a / b; }
}
