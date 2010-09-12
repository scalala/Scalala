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
 * Construction delegate for double(From).
 *
 * @author dramage
 */
trait CanNorm[-From] extends ((From,Double)=>Double);

object CanNorm {

  implicit def Tensor1Norm[T,K,V](implicit tt : Tensor1[T,K,V], cv : V=>Double)
  : CanNorm[T] = new CanNorm[T] {
    def apply(t : T, n : Double) : Double = {
      var norm = 0.0;

      if (n == 1.0) {
        tt.foreachNonZeroValue(t) { (v : V) => norm += scala.math.abs(v) }
      } else if (n == 2.0) {
        tt.foreachNonZeroValue(t) { (v : V) => norm += (v * v) }
        norm = scala.math.sqrt(norm);
      } else if (n % 2 == 0) {
        tt.foreachNonZeroValue(t) { (v : V) => norm += scala.math.pow(v, n) }
        norm = scala.math.pow(norm, 1.0 / n);
      } else if (n == Double.PositiveInfinity) {
        tt.foreachNonZeroValue(t) { (v : V) => norm = scala.math.max(norm, scala.math.abs(v)); }
      } else if (n > 0) {
        tt.foreachNonZeroValue(t) { (v : V) => norm += scala.math.pow(scala.math.abs(v), n) }
        norm = scala.math.pow(norm, 1.0 / n);
      } else {
        throw new UnsupportedOperationException();
      }

      norm;
    }
  }
}
