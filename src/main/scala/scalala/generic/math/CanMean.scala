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

/**
 * Construction delegate for mean(From).
 * 
 * @author dramage
 */
trait CanMean[-From,+To] extends (From=>To);

trait LowPriorityCanMean {
  /** Computes the mean by adding and dividing.  This is slower than the implementation in ZeroInto. */
  implicit def AddDiv[V](implicit add : BinaryOp[V,V,OpAdd,V], div : BinaryOp[V,Int,OpDiv,V])
  : CanMean[Traversable[V],V] = new CanMean[Traversable[V],V] {
    def apply(values : Traversable[V]) = {
      var current = values.head;
      var n = 0;
      for (value <- values) {
        if (n >= 1) {
          current = add(current, value);
        }
        n += 1;
      }
      div(current, n);
    }
  }
}

object CanMean { // extends LowPriorityCanMean {
  /** Numerically stable one-pass mean computation. */
  implicit def TraversableOnceMeanScalar[S](implicit view : S=>Double)
  : CanMean[TraversableOnce[S],Double]
  = new CanMean[TraversableOnce[S],Double] {
    def apply(values : TraversableOnce[S]) = {
      var m = 0.0;
      var k = 0;
      for (x <- values) {
        k += 1;
        m += (x - m) / k;
      }
      m;
    }
  }
  
  /** Optimized implementation for array of doubles. */
  implicit def ArrayMeanScalar[S](implicit view : S=>Double)
  : CanMean[Array[S],Double]
  = new CanMean[Array[S],Double] {
    def apply(values : Array[S]) = {
      var m = 0.0;
      var k = 0;
      while (k < values.length) {
        m += (values(k) - m) / (k + 1);
        k += 1;
      }
      m;
    }
  }

  implicit object TraversableMean extends CanMean[Traversable[Double],Double] {
    def apply(values : Traversable[Double]) = {
      values.sum / values.size;
    }
  }

  /** Computes the mean by starting with zero, adding into it, and dividing into it. */
  implicit def ZeroInto[V,RV](implicit zero : CanCreateZerosLike[V,RV],
   addInto : BinaryUpdateOp[RV,V,OpAdd], divInto : BinaryUpdateOp[RV,Int,OpDiv])
  : CanMean[TraversableOnce[V],RV] = new CanMean[TraversableOnce[V],RV] {
    def apply(values : TraversableOnce[V]) : RV = {
      var sum : RV = null.asInstanceOf[RV];
      var k = 0;
      for (value <- values) {
        if (k == 0) {
          sum = zero(value);
        }
        addInto(sum, value);
        k += 1;
      }
      divInto(sum, k);
      sum;
    }
  }
}

