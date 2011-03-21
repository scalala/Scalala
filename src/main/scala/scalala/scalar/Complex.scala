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

import scalala.operators._;

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
    case real : Double => this.real == real && this.imag == 0;
    case real : Int => this.real == real && this.imag == 0;
    case real : Short => this.real == real && this.imag == 0;
    case real : Long => this.real == real && this.imag == 0;
    case real : Float => this.real == real && this.imag == 0;
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
      throw new UnsupportedOperationException("Cannot automatically convert complex numbers to doubles");
    
    def isNaN(a : Complex) =
      a.real.isNaN || a.imag.isNaN;
    
    val manifest = implicitly[ClassManifest[Complex]];
    
    val defaultArrayValue =
      implicitly[scalala.collection.sparse.DefaultArrayValue[Complex]];
  }

  //
  // neg
  //

  implicit object Neg extends UnaryOp[Complex,OpNeg,Complex]
    { def apply(v : Complex) = -v; }

  //
  // cast
  //
  
  implicit object CastIC extends UnaryOp[Int,OpCast,Complex]
    { def apply(v : Int) = Complex(v,0); }

  implicit object CastLC extends UnaryOp[Long,OpCast,Complex]
    { def apply(v : Long) = Complex(v,0); }

  implicit object CastFC extends UnaryOp[Float,OpCast,Complex]
    { def apply(v : Float) = Complex(v,0); }

  implicit object CastDC extends UnaryOp[Double,OpCast,Complex]
    { def apply(v : Double) = Complex(v,0); }

  //
  // add
  //

  implicit object AddCC extends BinaryOp[Complex,Complex,OpAdd,Complex]
  { def apply(a : Complex, b : Complex) = a + b; }

  implicit object AddIC extends BinaryOp[Int,Complex,OpAdd,Complex]
  { def apply(a : Int, b : Complex) = a + b; }

  implicit object AddLC extends BinaryOp[Long,Complex,OpAdd,Complex]
  { def apply(a : Long, b : Complex) = a + b; }

  implicit object AddFC extends BinaryOp[Float,Complex,OpAdd,Complex]
  { def apply(a : Float, b : Complex) = a + b; }

  implicit object AddDC extends BinaryOp[Double,Complex,OpAdd,Complex]
  { def apply(a : Double, b : Complex) = a + b; }

  implicit object AddCI extends BinaryOp[Complex,Int,OpAdd,Complex]
  { def apply(a : Complex, b : Int) = a + b; }

  implicit object AddCL extends BinaryOp[Complex,Long,OpAdd,Complex]
  { def apply(a : Complex, b : Long) = a + b; }

  implicit object AddCF extends BinaryOp[Complex,Float,OpAdd,Complex]
  { def apply(a : Complex, b : Float) = a + b; }

  implicit object AddCD extends BinaryOp[Complex,Double,OpAdd,Complex]
  { def apply(a : Complex, b : Double) = a + b; }

  //
  // sub
  //

  implicit object SubCC extends BinaryOp[Complex,Complex,OpSub,Complex]
  { def apply(a : Complex, b : Complex) = a - b; }

  implicit object SubIC extends BinaryOp[Int,Complex,OpSub,Complex]
  { def apply(a : Int, b : Complex) = a - b; }

  implicit object SubLC extends BinaryOp[Long,Complex,OpSub,Complex]
  { def apply(a : Long, b : Complex) = a - b; }

  implicit object SubFC extends BinaryOp[Float,Complex,OpSub,Complex]
  { def apply(a : Float, b : Complex) = a - b; }

  implicit object SubDC extends BinaryOp[Double,Complex,OpSub,Complex]
  { def apply(a : Double, b : Complex) = a - b; }

  implicit object SubCI extends BinaryOp[Complex,Int,OpSub,Complex]
  { def apply(a : Complex, b : Int) = a - b; }

  implicit object SubCL extends BinaryOp[Complex,Long,OpSub,Complex]
  { def apply(a : Complex, b : Long) = a - b; }

  implicit object SubCF extends BinaryOp[Complex,Float,OpSub,Complex]
  { def apply(a : Complex, b : Float) = a - b; }

  implicit object SubCD extends BinaryOp[Complex,Double,OpSub,Complex]
  { def apply(a : Complex, b : Double) = a - b; }

  //
  // mul
  //

  implicit object MulCC extends BinaryOp[Complex,Complex,OpMul,Complex]
  { def apply(a : Complex, b : Complex) = a * b; }

  implicit object MulIC extends BinaryOp[Int,Complex,OpMul,Complex]
  { def apply(a : Int, b : Complex) = a * b; }

  implicit object MulLC extends BinaryOp[Long,Complex,OpMul,Complex]
  { def apply(a : Long, b : Complex) = a * b; }

  implicit object MulFC extends BinaryOp[Float,Complex,OpMul,Complex]
  { def apply(a : Float, b : Complex) = a * b; }

  implicit object MulDC extends BinaryOp[Double,Complex,OpMul,Complex]
  { def apply(a : Double, b : Complex) = a * b; }

  implicit object MulCI extends BinaryOp[Complex,Int,OpMul,Complex]
  { def apply(a : Complex, b : Int) = a * b; }

  implicit object MulCL extends BinaryOp[Complex,Long,OpMul,Complex]
  { def apply(a : Complex, b : Long) = a * b; }

  implicit object MulCF extends BinaryOp[Complex,Float,OpMul,Complex]
  { def apply(a : Complex, b : Float) = a * b; }

  implicit object MulCD extends BinaryOp[Complex,Double,OpMul,Complex]
  { def apply(a : Complex, b : Double) = a * b; }

  //
  // div
  //

  implicit object DivCC extends BinaryOp[Complex,Complex,OpDiv,Complex]
  { def apply(a : Complex, b : Complex) = a / b; }

  implicit object DivIC extends BinaryOp[Int,Complex,OpDiv,Complex]
  { def apply(a : Int, b : Complex) = a / b; }

  implicit object DivLC extends BinaryOp[Long,Complex,OpDiv,Complex]
  { def apply(a : Long, b : Complex) = a / b; }

  implicit object DivFC extends BinaryOp[Float,Complex,OpDiv,Complex]
  { def apply(a : Float, b : Complex) = a / b; }

  implicit object DivDC extends BinaryOp[Double,Complex,OpDiv,Complex]
  { def apply(a : Double, b : Complex) = a / b; }

  implicit object DivCI extends BinaryOp[Complex,Int,OpDiv,Complex]
  { def apply(a : Complex, b : Int) = a / b; }

  implicit object DivCL extends BinaryOp[Complex,Long,OpDiv,Complex]
  { def apply(a : Complex, b : Long) = a / b; }

  implicit object DivCF extends BinaryOp[Complex,Float,OpDiv,Complex]
  { def apply(a : Complex, b : Float) = a / b; }

  implicit object DivCD extends BinaryOp[Complex,Double,OpDiv,Complex]
  { def apply(a : Complex, b : Double) = a / b; }
}

