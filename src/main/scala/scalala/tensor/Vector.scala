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

trait VectorLike[+This<:Vector]
extends MutableDomainSeqLike[Double,This]
with Tensor1Like[Int,IndexDomain,This];

trait Vector
extends MutableDomainSeq[Double]
with Tensor1[Int]
with VectorLike[Vector];

object Vector {
  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
  trait SliceSeqLike
  [@specialized(Int,Long) A, D<:IterableDomain[A] with DomainLike[A,D],
   +Coll <: MutableDomainMap[A,Double],
   +This <: SliceSeq[A, Coll]]
  extends MutableDomainMapSliceSeqLike[A,D,Double,Coll,This]
  with VectorLike[This];

  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
  trait SliceSeq
  [@specialized(Int,Long) A, +Coll <: MutableDomainMap[A,Double]]
  extends MutableDomainMapSliceSeq[A,Double,Coll]
  with Vector with SliceSeqLike[A,IterableDomain[A],Coll,SliceSeq[A,Coll]];

  class SliceFromKeySeq
  [@specialized(Int,Long) A, +Coll <: MutableDomainMap[A,Double]]
  (underlying : Coll, keys : Seq[A])
  extends MutableDomainMapSliceSeq.FromKeySeq[A,Double,Coll](underlying, keys)
  with SliceSeq[A,Coll];
}
