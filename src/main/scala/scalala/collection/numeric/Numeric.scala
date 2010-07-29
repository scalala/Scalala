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
package scalala.collection.numeric

/**
 * Abstract arithmetic operations on type V.  Like scala.Numeric but
 * with more specificity for scalala requirements (like being able to
 * divide on all types).
 *
 * @author dramage
 */
trait Numeric[@specialized(Int,Short,Long,Float,Double) V] {
  def zero : V;
  def one : V;
  def >(a : V, b : V) : Boolean;
  def >=(a : V, b : V) : Boolean;
  def <(a : V, b : V) : Boolean;
  def <=(a : V, b : V) : Boolean;
  def +(a : V, b : V) : V;
  def -(a : V, b : V) : V;
  def *(a : V, b : V) : V;
  def /(a : V, b : V) : V;
  def %(a : V, b : V) : V;
  def pow(a : V, b : V) : V;
}

object Numeric {
  implicit object IntNumeric extends Numeric[Int] {
    override def zero = 0;
    override def one  = 1;
    override def >(a : Int, b : Int) = a > b;
    override def >=(a : Int, b : Int) = a >= b;
    override def <(a : Int, b : Int) = a < b;
    override def <=(a : Int, b : Int) = a <= b;
    override def +(a : Int, b : Int) = a + b;
    override def -(a : Int, b : Int) = a - b;
    override def *(a : Int, b : Int) = a * b;
    override def /(a : Int, b : Int) = a / b;
    override def %(a : Int, b : Int) = a % b;
    override def pow(a : Int, b : Int) : Int = {
      if (b == 0) {
        return 1;
      }

      if (b < 0) {
        throw new IllegalArgumentException("Cannot raise to negative Int power");
      }

      var i = 1;
      var rv = a;
      while (i < b) {
        rv *= a;
        i += 1;
      }
      rv;
    }
  }

  implicit object LongNumeric extends Numeric[Long] {
    override def zero = 0l;
    override def one  = 1l;
    override def >(a : Long, b : Long) = a > b;
    override def >=(a : Long, b : Long) = a >= b;
    override def <(a : Long, b : Long) = a < b;
    override def <=(a : Long, b : Long) = a <= b;
    override def +(a : Long, b : Long) = a + b;
    override def -(a : Long, b : Long) = a - b;
    override def *(a : Long, b : Long) = a * b;
    override def /(a : Long, b : Long) = a / b;
    override def %(a : Long, b : Long) = a % b;
    override def pow(a : Long, b : Long) : Long = {
      if (b == 0) {
        return 1;
      }

      if (b < 0) {
        throw new IllegalArgumentException("Cannot raise to negative Long power");
      }

      var i = 1;
      var rv = a;
      while (i < b) {
        rv *= a;
        i += 1;
      }
      rv;
    }
  }

  implicit object FloatNumeric extends Numeric[Float] {
    override def zero = 0f;
    override def one  = 1f;
    override def >(a : Float, b : Float) = a > b;
    override def >=(a : Float, b : Float) = a >= b;
    override def <(a : Float, b : Float) = a < b;
    override def <=(a : Float, b : Float) = a <= b;
    override def +(a : Float, b : Float) = a + b;
    override def -(a : Float, b : Float) = a - b;
    override def *(a : Float, b : Float) = a * b;
    override def /(a : Float, b : Float) = a / b;
    override def %(a : Float, b : Float) = a % b;
    override def pow(a : Float, b : Float) : Float = math.pow(a, b).toFloat;
  }

  implicit object DoubleNumeric extends Numeric[Double] {
    override def zero = 0.0;
    override def one  = 1.0;
    override def >(a : Double, b : Double) = a > b;
    override def >=(a : Double, b : Double) = a >= b;
    override def <(a : Double, b : Double) = a < b;
    override def <=(a : Double, b : Double) = a <= b;
    override def +(a : Double, b : Double) = a + b;
    override def -(a : Double, b : Double) = a - b;
    override def *(a : Double, b : Double) = a * b;
    override def /(a : Double, b : Double) = a / b;
    override def %(a : Double, b : Double) = a % b;
    override def pow(a : Double, b : Double) : Double = math.pow(a, b);
  }
}
