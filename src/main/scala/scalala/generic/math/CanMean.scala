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

/**
 * Construction delegate for mean(From).
 * 
 * @author dramage
 */
trait CanMean[-From,+To] extends (From=>To);

trait LowPriorityCanMean {
  /** Computes the mean by adding and dividing.  This is slower than the implementation in ZeroInto. */
  implicit def AddDiv[V](implicit add : CanAdd[V,V,V], div : CanDiv[V,Int,V])
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
  implicit object ArrayMeanD extends CanMean[Array[Double],Double] {
    def apply(values : Array[Double]) = {
      var sum = 0.0;
      var i = 0;
      while (i < values.length) {
        sum += values(i);
        i += 1;
      }
      sum / values.length;
    }
  }

  /** Computes the mean by starting with zero, adding into it, and dividing into it. */
  implicit def ZeroInto[V,RV](implicit zero : CanZero[V,RV], addInto : CanAddInto[RV,V], divInto : CanDivInto[RV,Int])
  : CanMean[Traversable[V],RV] = new CanMean[Traversable[V],RV] {
    def apply(values : Traversable[V]) : RV = {
      val sum = zero(values.head);
      var n = 0;
      for (value <- values) {
        addInto(sum, value);
        n += 1;
      }
      divInto(sum, n);
      sum;
    }
  }
}
