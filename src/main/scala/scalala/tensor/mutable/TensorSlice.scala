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
[@specialized(Int,Long) A1, +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 @specialized(Int,Long) A2, +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:Tensor[A1,B],
 +This<:TensorSlice[A1,A2,B,Coll]]
extends tensor.TensorSliceLike[A1,D1,A2,D2,B,Coll,This]
with TensorLike[A2,B,D2,This] {

  override def update(key : A2, value : B) =
    underlying.update(lookup(key), value);
}

/**
 * Pass-through view of a (key-mapped) subset of an underlying Tensor.
 *
 * @author dramage
 */
trait TensorSlice
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll <: Tensor[A1, B]]
extends tensor.TensorSlice[A1,A2,B,Coll] with Tensor[A2,B]
with TensorSliceLike[A1, IterableDomain[A1], A2, IterableDomain[A2], B, Coll, TensorSlice[A1, A2, B, Coll]];


object TensorSlice {
  class FromKeyMap[A1, A2, B:Scalar, +Coll<:Tensor[A1, B]]
  (underlying : Coll, keymap : scala.collection.Map[A2,A1])
  extends tensor.TensorSlice.FromKeyMap[A1,A2,B,Coll](underlying, keymap)
  with TensorSlice[A1,A2,B,Coll];
}
