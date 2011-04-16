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

import scalala.collection.sparse.{SparseArray,DefaultArrayValue};
import scalala.scalar.Scalar;

/**
 * Marker for being able to create a collection of the same shape as
 * the given input but with zero values everywhere.
 *
 * @author dramage
 */
trait CanCreateZerosLike[-From, +To] extends (From=>To);

object CanCreateZerosLike {

  class OpArray[@specialized V:ClassManifest:Scalar]
  extends CanCreateZerosLike[Array[V],Array[V]] {
    override def apply(from : Array[V]) = {
      Array.fill(from.length)(implicitly[Scalar[V]].zero);
    }
  }

  class OpMapValues[From,A,To](implicit op : Scalar[A], map : CanMapValues[From,A,A,To]) extends CanCreateZerosLike[From,To] {
    def apply(v : From) = map.map(v, _ => op.zero);
  }

  implicit def opMapValues[From,A,To](implicit map : CanMapValues[From,A,A,To], op : Scalar[A])
  : CanCreateZerosLike[From,To] = new OpMapValues[From,A,To]()(op, map);

  implicit def OpArrayAny[V:ClassManifest:Scalar] : OpArray[V] =
    new OpArray[V];

  implicit object OpArrayI extends OpArray[Int];
  implicit object OpArrayS extends OpArray[Short];
  implicit object OpArrayL extends OpArray[Long];
  implicit object OpArrayF extends OpArray[Float];
  implicit object OpArrayD extends OpArray[Double];
}

