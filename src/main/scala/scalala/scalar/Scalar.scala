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
 * Marker trait for scalar values.  Scalars must be immutable.
 *
 * @author dramage
 */
trait Scalar[@specialized(Int,Short,Long,Float,Double) V] {
  def zero : V;

  def one : V;

  def ==(a : V, b : V) : Boolean;

  def !=(a : V, b : V) : Boolean;

  def >(a : V, b : V) : Boolean;

  def >=(a : V, b : V) : Boolean;

  def <(a : V, b : V) : Boolean;

  def <=(a : V, b : V) : Boolean;

  def +(a : V, b : V) : V;

  def -(a : V, b : V) : V;

  def *(a : V, b : V) : V;

  def /(a : V, b : V) : V;

  /** Returns the norm of this value, the absolute value as a Double. */
  def norm(a : V) : Double;

  /** Returns this value as a Double.  May throw UnsupportedOperationException. */
  def toDouble(a : V) : Double;

  /** Returns true if this is not a number. */
  def isNaN(a : V) : Boolean;

  /** Returns the class manifest of the scalar type. */
  def manifest : ClassManifest[V];
}

object Scalar {
  implicit object ScalarI extends Scalar[Int] {
    def zero = 0;
    def one = 1;
    def ==(a : Int, b : Int) = a == b;
    def !=(a : Int, b : Int) = a != b;
    def >(a : Int, b : Int) = a > b;
    def >=(a : Int, b : Int) = a >= b;
    def <(a : Int, b : Int) = a < b;
    def <=(a : Int, b : Int) = a <= b;
    def +(a : Int, b : Int) = a + b;
    def -(a : Int, b : Int) = a - b;
    def *(a : Int, b : Int) = a * b;
    def /(a : Int, b : Int) = a / b;
    def norm(a : Int) = if (a < 0) -a else a;
    def toDouble(a : Int) = a;
    def isNaN(a : Int) = false;
    val manifest = implicitly[ClassManifest[Int]];
  }

  implicit object ScalarS extends Scalar[Short] {
    def zero = 0.asInstanceOf[Short];
    def one = 1.asInstanceOf[Short];
    def ==(a : Short, b : Short) = a == b;
    def !=(a : Short, b : Short) = a != b;
    def >(a : Short, b : Short) = a > b;
    def >=(a : Short, b : Short) = a >= b;
    def <(a : Short, b : Short) = a < b;
    def <=(a : Short, b : Short) = a <= b;
    def +(a : Short, b : Short) = (a + b).asInstanceOf[Short];
    def -(a : Short, b : Short) = (a - b).asInstanceOf[Short];
    def *(a : Short, b : Short) = (a * b).asInstanceOf[Short];
    def /(a : Short, b : Short) = (a / b).asInstanceOf[Short];
    def norm(a : Short) = if (a < 0) -a else a;
    def toDouble(a : Short) = a;
    def isNaN(a : Short) = false;
    val manifest = implicitly[ClassManifest[Short]];
  }

  implicit object ScalarL extends Scalar[Long] {
    def zero = 0l;
    def one = 1l;
    def ==(a : Long, b : Long) = a == b;
    def !=(a : Long, b : Long) = a != b;
    def >(a : Long, b : Long) = a > b;
    def >=(a : Long, b : Long) = a >= b;
    def <(a : Long, b : Long) = a < b;
    def <=(a : Long, b : Long) = a <= b;
    def +(a : Long, b : Long) = a + b;
    def -(a : Long, b : Long) = a - b;
    def *(a : Long, b : Long) = a * b;
    def /(a : Long, b : Long) = a / b;
    def norm(a : Long) = if (a < 0) -a else a;
    def toDouble(a : Long) = a;
    def isNaN(a : Long) = false;
    val manifest = implicitly[ClassManifest[Long]];
  }

  implicit object scalarF extends Scalar[Float] {
    def zero = 0.0f;
    def one = 1.0f;
    def ==(a : Float, b : Float) = a == b;
    def !=(a : Float, b : Float) = a != b;
    def >(a : Float, b : Float) = a > b;
    def >=(a : Float, b : Float) = a >= b;
    def <(a : Float, b : Float) = a < b;
    def <=(a : Float, b : Float) = a <= b;
    def +(a : Float, b : Float) = a + b;
    def -(a : Float, b : Float) = a - b;
    def *(a : Float, b : Float) = a * b;
    def /(a : Float, b : Float) = a / b;
    def norm(a : Float) = if (a < 0) -a else a;
    def toDouble(a : Float) = a;
    def isNaN(a : Float) = java.lang.Float.isNaN(a);
    val manifest = implicitly[ClassManifest[Float]];
  }

  implicit object scalarD extends Scalar[Double] {
    def zero = 0.0;
    def one = 1.0;
    def ==(a : Double, b : Double) = a == b;
    def !=(a : Double, b : Double) = a != b;
    def >(a : Double, b : Double) = a > b;
    def >=(a : Double, b : Double) = a >= b;
    def <(a : Double, b : Double) = a < b;
    def <=(a : Double, b : Double) = a <= b;
    def +(a : Double, b : Double) = a + b;
    def -(a : Double, b : Double) = a - b;
    def *(a : Double, b : Double) = a * b;
    def /(a : Double, b : Double) = a / b;
    def norm(a : Double) = if (a < 0) -a else a;
    def toDouble(a : Double) = a;
    def isNaN(a : Double) = java.lang.Double.isNaN(a);
    val manifest = implicitly[ClassManifest[Double]];
  }

  implicit object scalarB extends Scalar[Boolean] {
    def zero = false;
    def one = true;
    def ==(a : Boolean, b : Boolean) = a == b;
    def !=(a : Boolean, b : Boolean) = a != b;
    def >(a : Boolean, b : Boolean) = a > b;
    def >=(a : Boolean, b : Boolean) = a >= b;
    def <(a : Boolean, b : Boolean) = a < b;
    def <=(a : Boolean, b : Boolean) = a <= b;
    def +(a : Boolean, b : Boolean) = throw new UnsupportedOperationException();
    def -(a : Boolean, b : Boolean) = throw new UnsupportedOperationException();
    def *(a : Boolean, b : Boolean) = throw new UnsupportedOperationException();
    def /(a : Boolean, b : Boolean) = throw new UnsupportedOperationException();
    def norm(a : Boolean) = if (a) 1.0 else 0.0;
    def toDouble(a : Boolean) = if (a) 1.0 else 0.0;
    def isNaN(a : Boolean) = false;
    def manifest = implicitly[ClassManifest[Boolean]];
  }
}
