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
 * Marker trait for tensors indexed by a pair of keys.
 *
 * @author dramage
 */
trait Tensor2[Coll, @specialized(Int,Long) K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double,Boolean) +V] {
  def zero(from : Coll) : Coll;

  def domain(coll : Coll) : Product2Domain[K1,K2];

  def get(coll : Coll, k1 : K1, k2 : K2) : V;
}

object Tensor2 {
  implicit def arrayTensor2[@specialized V:ClassManifest:Scalar] =
    new ArrayTensor2[V]();

  class ArrayTensor2[@specialized V:ClassManifest:Scalar] extends Tensor2[Array[Array[V]],Int,Int,V] {
    def zero(from : Array[Array[V]]) =
      Array.fill(from.length, from(0).length)(implicitly[Scalar[V]].zero);

    def domain(coll : Array[Array[V]]) =
      TableDomain(coll.length, coll(0).length);
    
    def get(coll : Array[Array[V]], k1 : Int, k2 : Int) =
      coll(k1)(k2);
  }

  implicit object ArrayI extends ArrayTensor2[Int];
  implicit object ArrayS extends ArrayTensor2[Short];
  implicit object ArrayL extends ArrayTensor2[Long];
  implicit object ArrayF extends ArrayTensor2[Float];
  implicit object ArrayD extends ArrayTensor2[Double];
  implicit object ArrayB extends ArrayTensor2[Boolean];
}
