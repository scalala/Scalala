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

import scalar.Scalar;

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
 @specialized(Int,Long,Float,Double,Boolean) V,
 +Coll<:Tensor[K1,V],
 +This<:TensorSlice[K1,K2,V,Coll]]
extends tensor.TensorSliceLike[K1,D1,K2,D2,V,Coll,This]
with TensorLike[K2,V,D2,This] {

  override def update(key : K2, value : V) =
    underlying.update(lookup(key), value);
}

/**
 * Pass-through view of a (key-mapped) subset of an underlying Tensor.
 *
 * @author dramage
 */
trait TensorSlice
[@specialized(Int,Long) K1, @specialized(Int,Long) K2,
 @specialized(Int,Long,Float,Double,Boolean) V, +Coll <: Tensor[K1, V]]
extends tensor.TensorSlice[K1,K2,V,Coll] with Tensor[K2,V]
with TensorSliceLike[K1, IterableDomain[K1], K2, IterableDomain[K2], V, Coll, TensorSlice[K1, K2, V, Coll]];


object TensorSlice {
  class FromKeyMap[K1, K2, V:Scalar, +Coll<:Tensor[K1, V]]
  (underlying : Coll, keymap : scala.collection.Map[K2,K1])
  extends tensor.TensorSlice.FromKeyMap[K1,K2,V,Coll](underlying, keymap)
  with TensorSlice[K1,K2,V,Coll];
}

