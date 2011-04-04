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

import scalar.Scalar;

import domain.{IterableDomain,SetDomain};

/**
 * Implementation trait for slices of an underlying Tensor.  A slice
 * is a pass-through view of a (mapped) subset of the original Tensor's
 * domain.
 *
 * @author dramage
 */
trait TensorSliceLike
[@specialized(Int,Long) K1, +D1<:IterableDomain[K1],
 @specialized(Int,Long) K2, +D2<:IterableDomain[K2],
 @specialized(Int,Long,Float,Double,Boolean) V, +Coll<:Tensor[K1,V],
 +This<:TensorSlice[K1,K2,V,Coll]]
extends TensorLike[K2,V,D2,This] {
self =>

  /** The collection underlying this view. */
  protected def underlying: Coll;

  /** Maps the keys of this domain map to the keys of the underlying maps's. */
  def lookup(key : K2) : K1;

  override def apply(key : K2) =
    underlying.apply(lookup(key));

  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) =
    underlying.newBuilder[K2,V2](domain);
}

/**
 * Pass-through view of a (key-mapped) subset of an underlying Tensor.
 *
 * @author dramage
 */
trait TensorSlice
[@specialized(Int,Long) K1, @specialized(Int,Long) K2,
 @specialized(Int,Long,Float,Double,Boolean) V, +Coll <: Tensor[K1, V]]
extends Tensor[K2,V]
with TensorSliceLike[K1, IterableDomain[K1], K2, IterableDomain[K2], V, Coll, TensorSlice[K1, K2, V, Coll]] {

}

object TensorSlice {
  class FromKeyMap[K1, K2, V, +Coll<:Tensor[K1, V]]
  (override val underlying : Coll, keymap : scala.collection.Map[K2,K1])
  (override implicit val scalar : Scalar[V])
  extends TensorSlice[K1, K2, V, Coll] {
    override def lookup(key : K2) = keymap(key);
    override val domain = new SetDomain(keymap.keySet);
  }
}

