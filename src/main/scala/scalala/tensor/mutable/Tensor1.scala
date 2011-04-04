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

import domain.Domain1;

import scalala.scalar.Scalar;

/**
 * Implementation trait for mutable Tensor1 instances.
 *
 * @author dramage
 */
trait Tensor1Like
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +D<:Domain1[K], +This<:Tensor1[K,V]]
extends tensor.Tensor1Like[K,V,D,This] with TensorLike[K,V,D,This];

/**
 * Mutable tensor.Tensor1.
 *
 * @author dramage
 */
trait Tensor1
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends tensor.Tensor1[K,V] with Tensor[K,V]
with Tensor1Like[K,V,Domain1[K],Tensor1[K,V]];

object Tensor1 {
  /** Constructs a closed-domain tensor for the given domain. */
  def apply[K,V:Scalar](domain : Domain1[K]) =
    Tensor1Col(domain);
}

