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
package operators;

/**
 * Marker trait for scalar values.
 * @author dramage
 */
trait Scalar[@specialized(Int,Short,Long,Float,Double,Boolean) V] {
  def zero : V;
}

object Scalar {
  implicit object ScalarI extends Scalar[Int] {
    def zero = 0;
  }

  implicit object ScalarS extends Scalar[Short] {
    def zero = 0.asInstanceOf[Short];
  }

  implicit object ScalarL extends Scalar[Long] {
    def zero = 0l;
  }

  implicit object scalarF extends Scalar[Float] {
    def zero = 0.0f;
  }

  implicit object scalarD extends Scalar[Double] {
    def zero = 0.0;
  }

  implicit object scalarB extends Scalar[Boolean] {
    def zero = false;
  }
}
