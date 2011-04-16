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
 * Marker for being able to copy a collection
 *
 * @author dlwh
 */
trait CanCopy[T] extends (T=>T);

object CanCopy {

  class OpArray[@specialized V:ClassManifest:Scalar]
  extends CanCreateZerosLike[Array[V],Array[V]] {
    override def apply(from : Array[V]) = {
      Array.fill(from.length)(implicitly[Scalar[V]].zero);
    }
  }

  class OpMapValues[From,A](implicit op : CanCopy[A], map : CanMapValues[From,A,A,From]) extends CanCopy[From] {
    def apply(v : From) = map.map(v, op);
  }

  implicit def opMapValues[From,A](implicit map : CanMapValues[From,A,A,From], op : CanCopy[A])
  : CanCopy[From] = new OpMapValues[From,A]()(op, map);

  implicit def OpArrayAny[V:ClassManifest:Scalar] : OpArray[V] =
    new OpArray[V];

  implicit object OpArrayI extends OpArray[Int];
  implicit object OpArrayS extends OpArray[Short];
  implicit object OpArrayL extends OpArray[Long];
  implicit object OpArrayF extends OpArray[Float];
  implicit object OpArrayD extends OpArray[Double];

  implicit def canCopyScalar[V:Scalar]:CanCopy[V] = new CanCopy[V] {
    def apply(v1: V) = v1;
  }
}

