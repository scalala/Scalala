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

import domain.{IterableDomain,Domain1};

/**
 * Implementation trait for a Tensor1 view of a slice of keys from a Tensor.
 *
 * @author dramage
 */
trait Tensor1SliceLike
[@specialized(Int,Long) K1, +D1<:IterableDomain[K1],
 @specialized(Int,Long) K2, +D2<:Domain1[K2],
 @specialized(Int,Long,Float,Double,Boolean) V, +Coll<:Tensor[K1,V],
 +This<:Tensor1Slice[K1,K2,V,Coll]]
extends TensorSliceLike[K1, D1, K2, D2, V, Coll, This]
with Tensor1Like[K2, V, D2, This];

/**
 * A Tensor1 view of a slice of keys from a Tensor.
 *
 * @author dramage
 */
trait Tensor1Slice
[@specialized(Int,Long) K1, @specialized(Int,Long) K2,
 @specialized(Int,Long,Float,Double,Boolean) V, +Coll<:Tensor[K1,V]]
extends TensorSlice[K1,K2,V,Coll] with Tensor1[K2,V]
with Tensor1SliceLike[K1, IterableDomain[K1], K2, Domain1[K2], V, Coll, Tensor1Slice[K1, K2, V, Coll]];

