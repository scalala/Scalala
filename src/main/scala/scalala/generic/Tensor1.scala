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

import scalala.collection.domain._;

/**
 * Marker trait for tensors indexed by a single key.
 *
 * @author dramage
 */
trait Tensor1[Coll, @specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) +V] {
  def zero(from : Coll) : Coll;

  def domain(coll : Coll) : IterableDomain[K];

  def get(coll : Coll, key : K) : V;

  def valuesIterator(coll : Coll) : Iterator[V] =
    domain(coll).iterator.map(k => get(coll, k));
}

object Tensor1 {
  implicit def arrayTensor1[@specialized V:ClassManifest:Scalar] =
    new ArrayTensor1[V]();

  class ArrayTensor1[@specialized V:ClassManifest:Scalar] extends Tensor1[Array[V],Int,V] {
    def zero(from : Array[V]) = Array.fill(from.length)(implicitly[Scalar[V]].zero);
    def domain(coll : Array[V]) = IndexDomain(coll.length);
    def get(coll : Array[V], key : Int) = coll(key);
  }

  implicit object ArrayI extends ArrayTensor1[Int];
  implicit object ArrayS extends ArrayTensor1[Short];
  implicit object ArrayL extends ArrayTensor1[Long];
  implicit object ArrayF extends ArrayTensor1[Float];
  implicit object ArrayD extends ArrayTensor1[Double];
  implicit object ArrayB extends ArrayTensor1[Boolean];
}
