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

/**
 * Implementation trait for mutable Tensor1 instances.
 *
 * @author dramage
 */
trait Tensor1Like
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B,
 +D<:IterableDomain[A] with DomainLike[A,D], +This<:Tensor1[A,B]]
extends tensor.Tensor1Like[A,B,D,This] with TensorLike[A,B,D,This] {

}

/**
 * Mutable tensor.Tensor1.
 *
 * @author dramage
 */
trait Tensor1
[@specialized(Int,Long)A, @specialized(Int,Long,Float,Double) B]
extends tensor.Tensor1[A,B] with Tensor[A,B]
with Tensor1Like[A,B,IterableDomain[A],Tensor1[A,B]];

object Tensor1 extends Tensor1Companion[Tensor1];

trait Tensor1Companion[Bound[K,V]<:Tensor1[K,V]]
extends tensor.Tensor1Companion[Bound] with TensorCompanion[Bound];
