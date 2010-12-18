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
package collection;

import scalala.scalar.Scalar;

/**
 * Builder trait for transposing a matrix.
 *
 * @author dramage
 */
trait CanTranspose[-From, +To] {
  def apply(in : From) : To;
}

object CanTranspose {
  class ArrayArrayTranspose[V:Scalar:Manifest] extends CanTranspose[Array[Array[V]], Array[Array[V]]] {
    override def apply(in : Array[Array[V]]) =
      Array.tabulate(in(0).length, in.length)((i,j) => in(j)(i));
  }

  implicit def mkArrayArrayTranspose[V:Scalar:Manifest] =
    new ArrayArrayTranspose[V];

  implicit object OpI extends ArrayArrayTranspose[Int];
  implicit object OpS extends ArrayArrayTranspose[Short];
  implicit object OpL extends ArrayArrayTranspose[Long];
  implicit object OpF extends ArrayArrayTranspose[Float];
  implicit object OpD extends ArrayArrayTranspose[Double];
}
