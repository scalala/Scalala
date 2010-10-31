
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
 * Implementation trait for a Vector view of a slice of keys from a Tensor.
 *
 * @author dramage
 */
trait VectorSliceLike
[@specialized(Int,Long) A, +D<:IterableDomain[A] with DomainLike[A,D],
 @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:Tensor[A,B],
 +This<:VectorSlice[A,B,Coll]]
extends Tensor1SliceLike[A, D, Int, IndexDomain, B, Coll, This]
with VectorLike[B, This];

/**
 * A Vector view of a slice of keys from a Tensor.
 *
 * @author dramage
 */
trait VectorSlice
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:Tensor[A, B]]
extends Tensor1Slice[A,Int,B,Coll] with Vector[B]
with VectorSliceLike[A, IterableDomain[A], B, Coll, VectorSlice[A, B, Coll]];

object VectorSlice {
  class FromKeySeq[A, B, +Coll <: Tensor[A, B]]
  (override val underlying : Coll, keys : Seq[A])
  (implicit override val scalar : Scalar[B])
  extends VectorSlice[A, B, Coll] {
    override def lookup(key : Int) = keys(key);
    override val domain = IndexDomain(keys.length);
  }
}
