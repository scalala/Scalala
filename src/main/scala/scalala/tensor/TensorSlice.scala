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

import domain._;

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
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll<:Tensor[A1,B],
 +This<:TensorSlice[A1,A2,B,Coll]]
extends TensorLike[A2,B,D2,This] {
self =>

  /** The collection underlying this view. */
  protected def underlying: Coll;

  /** Maps the keys of this domain map to the keys of the underlying maps's. */
  def lookup(key : A2) : A1;

  override def apply(key : A2) =
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
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll <: Tensor[A1, B]]
extends Tensor[A2,B]
with TensorSliceLike[A1, IterableDomain[A1], A2, IterableDomain[A2], B, Coll, TensorSlice[A1, A2, B, Coll]] {

}

object TensorSlice extends TensorSliceCompanion[TensorSliceCompanion.Bound] {
  class FromKeyMap[A1, A2, B, +Coll<:Tensor[A1, B]]
  (override val underlying : Coll, keymap : scala.collection.Map[A2,A1])
  (override implicit val scalar : Scalar[B])
  extends TensorSlice[A1, A2, B, Coll] {
    override def lookup(key : A2) = keymap(key);
    override val domain = new SetDomain(keymap.keySet);
  }
}

trait TensorSliceCompanion[Bound[K,V] <: TensorSlice[_,K,V,_]] extends TensorCompanion[Bound];

object TensorSliceCompanion {
  type Bound[K,V] = TensorSlice[_,K,V,_];
}
