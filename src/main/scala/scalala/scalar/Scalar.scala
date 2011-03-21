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

import scala.annotation.implicitNotFound;
import scalala.collection.sparse.DefaultArrayValue;

/**
 * Marker trait for scalar values.  Scalars must be immutable.
 *
 * @author dramage
 */
@implicitNotFound(msg="${V} is not a scalar value")
trait Scalar[@specialized(Int,Short,Long,Float,Double) V] {
  def zero : V;

  def one : V;

  def nan : V;

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

  def min(a : V, b : V) : V =
    if (<=(a,b)) a else b;

  def max(a : V, b : V) : V =
    if (>=(a,b)) a else b;

  /** Returns the norm of this value, the absolute value as a Double. */
  def norm(a : V) : Double;

  /** Returns this value as a Double.  May throw UnsupportedOperationException. */
  def toDouble(a : V) : Double;

  /** Returns true if this is not a number. */
  def isNaN(a : V) : Boolean;

  /** Returns the class manifest of the scalar type. */
  def manifest : ClassManifest[V];
  
  /** Returns the DefaultArrayValue for this type.  Always this.zero. */
  def defaultArrayValue : DefaultArrayValue[V];
}

object Scalar {
  implicit object ScalarI extends Scalar[Int] {
    def zero = 0;
    def one = 1;
    def nan = throw new ArithmeticException("Operation resulted in integer-valued NaN");
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
    val defaultArrayValue = implicitly[DefaultArrayValue[Int]];
  }

  implicit object ScalarS extends Scalar[Short] {
    def zero = 0.asInstanceOf[Short];
    def one = 1.asInstanceOf[Short];
    def nan = throw new ArithmeticException("Operation resulted in short-valued NaN");
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
    val defaultArrayValue = implicitly[DefaultArrayValue[Short]];
  }

  implicit object ScalarL extends Scalar[Long] {
    def zero = 0l;
    def one = 1l;
    def nan = throw new ArithmeticException("Operation resulted in long-valued NaN");
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
    val defaultArrayValue = implicitly[DefaultArrayValue[Long]];
  }

  implicit object scalarF extends Scalar[Float] {
    def zero = 0.0f;
    def one = 1.0f;
    def nan = Float.NaN;
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
    val defaultArrayValue = implicitly[DefaultArrayValue[Float]];
  }

  implicit object scalarD extends Scalar[Double] {
    def zero = 0.0;
    def one = 1.0;
    def nan = Double.NaN;
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
    val defaultArrayValue = implicitly[DefaultArrayValue[Double]];
  }

  implicit object scalarB extends Scalar[Boolean] {
    def zero = false;
    def one = true;
    def nan = throw new ArithmeticException("Operation resulted in boolean-valued NaN");
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
    val defaultArrayValue = implicitly[DefaultArrayValue[Boolean]];
  }

  /** The default array value for a scalar is zero. */
  implicit def defaultArrayValue[V:Scalar] : DefaultArrayValue[V] =
    implicitly[Scalar[V]].defaultArrayValue;
}

