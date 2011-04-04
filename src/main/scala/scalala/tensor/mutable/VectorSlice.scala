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

import domain.{IterableDomain,IndexDomain};

import scalala.scalar.Scalar;
import scalala.generic.collection._;
import scalala.operators._;

/**
 * Implementation trait for a Vector-like view of a mutable Tensor.
 *
 * @author dramage
 */
trait VectorSliceLike
[@specialized(Int,Long) K, +D<:IterableDomain[K],
 @specialized(Int,Long,Float,Double,Boolean) V,
 +Coll<:Tensor[K,V],
 +This<:VectorSlice[K,V,Coll]]
extends tensor.VectorSliceLike[K, D, V, Coll, This]
with Tensor1SliceLike[K, D, Int, IndexDomain, V, Coll, This]
with VectorLike[V, This];

/**
 * Vector-like view of a mutable Tensor.
 *
 * @author dramage
 */
trait VectorSlice
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
 +Coll<:Tensor[K, V]]
extends tensor.VectorSlice[K,V,Coll]
with Tensor1Slice[K,Int,V,Coll]
with Vector[V]
with VectorSliceLike[K, IterableDomain[K], V, Coll, VectorSlice[K, V, Coll]];

object VectorSlice {
  class FromKeySeq[K, V:Scalar, +Coll<:Tensor[K, V]]
  (underlying : Coll, keys : Seq[K])
  extends tensor.VectorSlice.FromKeySeq[K,V,Coll](underlying, keys)
  with VectorSlice[K,V,Coll];
}
