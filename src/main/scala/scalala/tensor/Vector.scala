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

import generic.Scalar;
import collection._;
import collection.domain._;

trait VectorLike[@specialized(Int,Long,Float,Double) B, +This<:Vector[B]]
extends MutableDomainSeqLike[B,This]
with Tensor1Like[Int,B,IndexDomain,This];

trait Vector[@specialized(Int,Long,Float,Double) B]
extends MutableDomainSeq[B]
with Tensor1[Int,B]
with VectorLike[B,Vector[B]];

object Vector {
  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
  trait SliceSeqLike
  [@specialized(Int,Long) A,
   @specialized(Int,Long,Float,Double) B,
   D<:IterableDomain[A] with DomainLike[A,D],
   +Coll <: MutableDomainMap[A,B],
   +This <: SliceSeq[A, B, Coll]]
  extends MutableDomainMapSliceSeqLike[A,D,B,Coll,This]
  with VectorLike[B,This];

  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
  trait SliceSeq
  [@specialized(Int,Long) A,
   @specialized(Int,Long,Float,Double) B,
   +Coll <: MutableDomainMap[A,B]]
  extends MutableDomainMapSliceSeq[A,B,Coll]
  with Vector[B] with SliceSeqLike[A,B,IterableDomain[A],Coll,SliceSeq[A,B,Coll]];

  class SliceFromKeySeq
  [@specialized(Int,Long) A,
   @specialized(Int,Long,Float,Double) B,
   +Coll <: MutableDomainMap[A,B]]
  (underlying : Coll, keys : Seq[A])
  (implicit override val scalar : Scalar[B])
  extends MutableDomainMapSliceSeq.FromKeySeq[A,B,Coll](underlying, keys)
  with SliceSeq[A,B,Coll];
}
