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

import collection._;
import collection.domain._;

/**
 * Implementation trait for a tensor indexed by two values; e.g. the basis
 * of a Matrix.
 *
 * @author dramage
 */
trait Tensor2Like
[@specialized(Int) A1, @specialized(Int) A2,
 @specialized(Int,Long,Float,Double) B,
 +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +This<:Tensor2[A1,A2,B]]
extends TensorLike[(A1,A2),B,D,This]
with MutableDomainMap2Like[A1,A2,B,D1,D2,D,T,This] {
}

/**
 * A tensor indexed by two values; e.g. the basis of a Matrix.
 *
 * @author dramage
 */
trait Tensor2
[@specialized(Int) A1, @specialized(Int) A2, @specialized(Int,Long,Float,Double) B]
extends Tensor[(A1,A2),B]
with MutableDomainMap2[A1,A2,B]
with Tensor2Like[A1,A2,B,IterableDomain[A1],IterableDomain[A2],Product2Domain[A1,A2],Product2Domain[A2,A1],Tensor2[A1,A2,B]];


object Tensor2 {
  /** A transpose of any Tensor2 is a Tensor2. */
  trait TransposeLike
  [@specialized(Int) A2, @specialized(Int) A1,
   @specialized(Int,Long,Float,Double) B,
   +D2<:IterableDomain[A2] with DomainLike[A2,D2],
   +D1<:IterableDomain[A1] with DomainLike[A1,D1],
   +T<:Product2DomainLike[A2,A1,D2,D1,D,T],
   +D<:Product2DomainLike[A1,A2,D1,D2,T,D],
   +Coll<:Tensor2[A1,A2,B],
   +This<:Transpose[A2,A1,B,Coll]]
  extends MutableDomainMap2TransposeLike[A2,A1,B,D2,D1,T,D,Coll,This]
  with Tensor2Like[A2,A1,B,D2,D1,T,D,This];

  /** A transpose of any Tensor2 is a Tensor2. */
  trait Transpose
  [@specialized(Int) A2, @specialized(Int) A1,
   @specialized(Int,Long,Float,Double) B,
   +Coll <: Tensor2[A1,A2,B]]
  extends MutableDomainMap2Transpose[A2,A1,B,Coll]
  with Tensor2[A2,A1,B]
  with TransposeLike[A2,A1,B,IterableDomain[A2],IterableDomain[A1],Product2Domain[A2,A1],Product2Domain[A1,A2],Coll,Transpose[A2,A1,B,Coll]];
}
