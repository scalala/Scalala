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
import generic.collection._;

import scalar.Scalar;

/**
 * Implementation trait for a transposed view of an underlying MutableTensor2.
 *
 * @author dramage
 */
trait Tensor2TransposeLike
[@specialized(Int) A2, @specialized(Int) A1,
 @specialized(Int,Long,Float,Double,Boolean) B,
 +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 +Coll<:Tensor2[A1,A2,B],
 +This<:Tensor2Transpose[A2,A1,B,Coll]]
extends tensor.Tensor2TransposeLike[A2,A1,B,D2,D1,T,D,Coll,This]
with TensorSliceLike[(A1,A2),D,(A2,A1),T,B,Coll,This]
with Tensor2Like[A2,A1,B,D2,D1,T,D,This] {
self =>

  override def update(i : A2, j : A1, value : B) =
    underlying.update(j, i, value);
}

/**
 * Transposed view of an undelrying MutableTensor2.
 *
 * @author dramage
 */
trait Tensor2Transpose
[@specialized(Int) A2, @specialized(Int) A1,
 @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll <: Tensor2[A1,A2,B]]
extends tensor.Tensor2Transpose[A2,A1,B,Coll]
with TensorSlice[(A1,A2),(A2,A1),B,Coll]
with Tensor2[A2,A1,B]
with Tensor2TransposeLike[A2,A1,B,IterableDomain[A2],IterableDomain[A1],Product2Domain[A2,A1],Product2Domain[A1,A2],Coll,Tensor2Transpose[A2,A1,B,Coll]];

object Tensor2Transpose {
  /** Default implementation. */
  class Impl[A2, A1, B:Scalar, +Coll <: Tensor2[A1,A2,B]](underlying : Coll)
  extends tensor.Tensor2Transpose.Impl[A2,A1,B,Coll](underlying)
  with Tensor2Transpose[A2,A1,B,Coll];
}
