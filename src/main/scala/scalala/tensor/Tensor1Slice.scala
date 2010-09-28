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

import domain._;

/**
 * Implementation trait for a Tensor1 view of a slice of keys from a Tensor.
 *
 * @author dramage
 */
trait Tensor1SliceLike
[@specialized(Int,Long) A1, +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 @specialized(Int,Long) A2, +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll<:Tensor[A1,B],
 +This<:Tensor1Slice[A1,A2,B,Coll]]
extends TensorSliceLike[A1, D1, A2, D2, B, Coll, This]
with Tensor1Like[A2, B, D2, This];

/**
 * A Tensor1 view of a slice of keys from a Tensor.
 *
 * @author dramage
 */
trait Tensor1Slice
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll<:Tensor[A1,B]]
extends TensorSlice[A1,A2,B,Coll] with Tensor1[A2,B]
with Tensor1SliceLike[A1, IterableDomain[A1], A2, IterableDomain[A2], B, Coll, Tensor1Slice[A1, A2, B, Coll]];
