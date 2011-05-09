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
package tensor;
package mutable;

import domain._;

import scalala.scalar.Scalar;
import scalala.generic.collection._
import tensor.Counter2.Curried
import scala.collection.mutable.HashMap
;


/**
 * A mutable Tensor2 with an open row key domain that maps to an arbitrary Tensor1 as its rows.
 *
 * CRS = Compress Row Storage
 *
 * @author dlwh
 */
trait CRSTensor2Like
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V,
 +M1[VV]<:Curried[scala.collection.mutable.Map,K1]#Result[VV],
 +T<:scalala.tensor.mutable.Tensor1[K2,V],
 +This<:CRSTensor2[K1,K2,V,T]]
extends tensor.CRSTensor2Like[K1,K2,V,M1,T,This] with Tensor2Like[K1,K2,V,SetDomain[K1], SetDomain[K2], Domain2[K1,K2], Domain2[K2,K1], This] { self =>

  def update(k1 : K1, k2: K2, v : V) =
    innerGetOrElseUpdate(k1,data)(k2) = v;

  private[mutable] def innerGetOrElseUpdate[M](k:K1, m: scala.collection.mutable.Map[K1,M]): M = {
    m.getOrElseUpdate(k,m.default(k))
  }
}

trait CRSTensor2
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V, +T<:Tensor1[K2,V]]
extends tensor.CRSTensor2[K1,K2,V,T] with Tensor2[K1,K2,V]
with CRSTensor2Like[K1,K2, V,Curried[scala.collection.mutable.Map,K1]#Result,T,CRSTensor2[K1,K2,V,T]];

object CRSTensor2 {
  class Impl[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V, T<:Tensor1[K2,V]]
  (override val data : scala.collection.mutable.Map[K1,T],
   override val k2domain: Domain1[K2])
  (implicit override val scalar : Scalar[V], zeros: CanCreateZerosLike[T,T])
  extends CRSTensor2[K1,K2,V,T];

  /**
   * Returns a new empty CRSTensor2 using the "template" to create rows. The actual rows are zero'd out versions of
   * the template.
   */
  def apply[K1,K2,V,T<:Tensor1[K2,V]](template: T)(implicit view: CanViewAsTensor1[T,K2,V],
                                                   zeros: CanCreateZerosLike[T,T],
                                                   scalar: Scalar[V]) : mutable.CRSTensor2[K1,K2,V,T] = {
    val map = new HashMap[K1,T] {
      override def default(k: K1) = zeros(template);
    }
    new Impl[K1,K2,V,T](map, template.domain);
  }

  implicit def canSliceRow[K1,K2,V,T<:Tensor1[K2,V]]
  : CanSliceRow[CRSTensor2[K1,K2,V,T],K1,T]
  = new CanSliceRow[CRSTensor2[K1,K2,V,T],K1,T] {
    override def apply(from : CRSTensor2[K1,K2,V,T], row : K1) = from.innerGetOrElseUpdate(row,from.data);
  }


}

