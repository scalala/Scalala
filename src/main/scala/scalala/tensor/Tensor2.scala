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
[@specialized A1,@specialized A2,D1<:IterableDomain[A1],D2<:IterableDomain[A2],
 D<:Product2Domain[A1,A2,D1,D2], +This]
extends TensorLike[(A1,A2),D,This]
with MutableDomainMap2Like[A1,A2,Double,D1,D2,D,This] {
}

/**
 * A tensor indexed by two values; e.g. the basis of a Matrix.
 *
 * @author dramage
 */
trait Tensor2[A1,A2,D1<:IterableDomain[A1],D2<:IterableDomain[A2],D<:Product2Domain[A1,A2,D1,D2]]
extends Tensor[(A1,A2),D]
with MutableDomainMap2[A1,A2,Double,D1,D2,D]
with Tensor2Like[A1,A2,D1,D2,D,Tensor2[A1,A2,D1,D2,D]];


object Tensor2 {
  /** A transpose of any Tensor2 is a Tensor2. */
  trait TransposeLike
  [@specialized A2, @specialized A1,
   D2<:IterableDomain[A2], D1<:IterableDomain[A1],
   ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
   +Coll <: Tensor2[A1,A2,D1,D2,ID],
   +This <: TransposeLike[A2,A1,D2,D1,ID,OD,Coll,This]]
  extends MutableDomainMap2TransposeLike[A2,A1,Double,D2,D1,ID,OD,Coll,This]
  with Tensor2Like[A2,A1,D2,D1,OD,This];

  /** A transpose of any Tensor2 is a Tensor2. */
  trait Transpose
  [@specialized A2, @specialized A1,
   D2<:IterableDomain[A2], D1<:IterableDomain[A1],
   ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
   +Coll <: Tensor2[A1,A2,D1,D2,ID]]
  extends MutableDomainMap2Transpose[A2,A1,Double,D2,D1,ID,OD,Coll]
  with Tensor2[A2,A1,D2,D1,OD]
  with TransposeLike[A2,A1,D2,D1,ID,OD,Coll,Transpose[A2,A1,D2,D1,ID,OD,Coll]];
}
