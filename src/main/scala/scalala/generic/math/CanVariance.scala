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
package generic;
package math;

import scalala.generic.collection.CanCreateZerosLike;
import scalala.operators._;
import scalala.tensor.Tensor;
import scalala.scalar.ScalarDecimal;

/**
 * Construction delegate for variance(From).
 * 
 * @author dramage
 */
trait CanVariance[-From,+To] extends (From=>To);

object CanVariance {
  /**
   * Numerically stable one-pass sample variance computation.
   *
   * From http://www.cs.berkeley.edu/~mhoemmen/cs194/Tutorials/variance.pdf
   */
  implicit def TraversableOnceVarianceScalar[S](implicit view : S=>Double)
  : CanVariance[TraversableOnce[S],Double]
  = new CanVariance[TraversableOnce[S],Double] {
    def apply(values : TraversableOnce[S]) = {
      var m = 0.0;
      var q = 0.0;
      var k = 0;
      for (x <- values) {
        k += 1;
        val xMm = x - m;
        val xMmDk = xMm / k;
        m = m + xMmDk;
        q = q + (k - 1) * xMm * xMmDk
      }
      q / (k - 1);
    }
  }
  
  implicit def ArrayVarianceScalar[S](implicit view : S=>Double)
  : CanVariance[Array[S],Double]
  = new CanVariance[Array[S],Double] {
    def apply(values : Array[S]) = {
      var m = 0.0;
      var q = 0.0;
      var k = 0;
      while (k < values.length) {
        val x = values(k);
        k += 1;
        val xMm = x - m;
        val xMmDk = xMm / k;
        m = m + xMmDk;
        q = q + (k - 1) * xMm * xMmDk
      }
      q / (k - 1);
    }
  }
  
  implicit def TensorVariance[T,V,D](implicit view : T=>Tensor[_,V],
    sd : ScalarDecimal[V,D],
    sub  : BinaryOp[V,D,OpSub,D],
    add  : BinaryOp[D,D,OpAdd,D],
    div  : BinaryOp[D,Int,OpDiv,D],
    mul1 : BinaryOp[D,D,OpMul,D],
    mul2 : BinaryOp[D,Int,OpMul,D])
  : CanVariance[T,D]
  = new CanVariance[T,D] {
    override def apply(tensor : T) = {
      var m = sd.decimal.zero;
      var q = sd.decimal.zero;
      var k = 0;
      // TODO: this could be more efficient by using foreachNonZeroValue
      tensor.foreachValue(x => {
        k += 1;
        val xMm = sub(x, m);
        val xMmDk = div(xMm, k);
        m = add(m, xMmDk);
        q = add(q, mul2(mul1(xMm,xMmDk), k-1));
      });
      div(q, k-1);
    }
  }
}

