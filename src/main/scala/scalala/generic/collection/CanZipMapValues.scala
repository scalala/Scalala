package scalala
package generic
package collection

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

import scalala.scalar.Complex
import scalala.collection.sparse.{SparseArray, DefaultArrayValue}
import tensor.domain.CanGetDomain
;

/**
 * Marker for being able to zip two From's and map the values to a new collection
 *
 * @author dlwh
 */
trait CanZipMapValues[From, @specialized A, @specialized B, +To] {
  /** Maps all corresponding values from the two collection. */
  def map(from : From, from2: From, fn : (A,A)=>B) : To;
}

object CanZipMapValues {
  type Op[From, A, B, To] = CanZipMapValues[From, A, B, To];

  //
  // Arrays
  //

  class OpArray[@specialized A, @specialized B: ClassManifest]
    extends Op[Array[A], A, B, Array[B]] {

    /**Maps all values from the given collection. */
    def map(from: Array[A], from2: Array[A], fn: (A, A) => B) = {
      require(from.length == from2.length, "Array lengths don't match!")
      val arr = new Array[B](from.length)
      for(i <- 0 until from.length) {
        arr(i) = fn(from(i), from2(i))
      }
      arr
    }

  }


  implicit def opArray[@specialized A, @specialized B: ClassManifest] =
    new OpArray[A, B];

  implicit object OpArrayII extends OpArray[Int, Int];

  implicit object OpArraySS extends OpArray[Short, Short];

  implicit object OpArrayLL extends OpArray[Long, Long];

  implicit object OpArrayFF extends OpArray[Float, Float];

  implicit object OpArrayDD extends OpArray[Double, Double];

  implicit object OpArrayCC extends OpArray[Complex, Complex];

  implicit object OpArrayID extends OpArray[Int, Double];

  implicit object OpArraySD extends OpArray[Short, Double];

  implicit object OpArrayLD extends OpArray[Long, Double];

  implicit object OpArrayFD extends OpArray[Float, Double];

  implicit def canZipMapFromJoin[From,K,V,Domain,V2,To](implicit canJoin: CanJoin[From, From, K, V, V],
                                                        dom: CanGetDomain[From, Domain],
                                                        cbf: CanBuildTensorFrom[From, Domain, K, V2, To]):CanZipMapValues[From, V, V2, To] = {
    new CanZipMapValues[From, V, V2, To] {
      /**Maps all corresponding values from the two collection. */
      def map(from: From, from2: From, fn: (V, V) => V2) = {
        val b = cbf(from, dom(from))
        canJoin.joinAll(from, from2, {(k,v,v2) => b.update(k, fn(v,v2))})
        b.result
      }
    }
  }

}
