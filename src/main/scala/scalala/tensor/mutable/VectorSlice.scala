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
import generic.tensor._;

/**
 * Implementation trait for a pass-through DomainSeq-like view of an underlying
 * MutableTensor.
 *
 * @author dramage
 */
trait VectorSliceLike
[@specialized(Int,Long) A1, +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:Tensor[A1,B],
 +This<:VectorSlice[A1,B,Coll]]
extends TensorSliceLike[A1, D1, Int, IndexDomain, B, Coll, This]
with tensor.VectorSliceLike[A1, D1, B, Coll, This]
with VectorLike[B, This];

/**
 * DomainSeq like view of an underlying MutableTensor.
 *
 * @author dramage
 */
trait VectorSlice
[@specialized(Int,Long) A1, @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:Tensor[A1, B]]
extends TensorSlice[A1,Int,B,Coll]
with tensor.VectorSlice[A1,B,Coll]
with Vector[B]
with VectorSliceLike[A1, IterableDomain[A1], B, Coll, VectorSlice[A1, B, Coll]];

object VectorSlice {
  class FromKeySeq[A, B:Scalar, +Coll<:Tensor[A, B]]
  (underlying : Coll, keys : Seq[A])
  extends tensor.VectorSlice.FromKeySeq[A,B,Coll](underlying, keys)
  with VectorSlice[A,B,Coll];
}
