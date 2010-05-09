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

trait VectorLike[+This]
extends MutableDomainSeqLike[Double,This]
with Tensor1Like[Int,IndexDomain,This];

trait Vector
extends MutableDomainSeq[Double]
with Tensor1[Int,IndexDomain]
with VectorLike[Vector];

object Vector {
  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
  trait SliceSeqLike
  [A, D<:IterableDomain[A], +Coll <: MutableDomainMap[A,Double,D],
   +This <: SliceSeqLike[A, D, Coll, This]]
  extends MutableDomainMapSliceSeqLike[A,D,Double,Coll,This]
  with VectorLike[This];

  /** A slice-seq of any Double-valued MutableDomainMap is a Vector. */
  trait SliceSeq
  [A, D<:IterableDomain[A], +Coll <: MutableDomainMap[A,Double,D]]
  extends MutableDomainMapSliceSeq[A,D,Double,Coll]
  with Vector with SliceSeqLike[A,D,Coll,SliceSeq[A,D,Coll]];

  class SliceFromKeySeq
  [@specialized A, D<:IterableDomain[A], +Coll<:MutableDomainMap[A, Double, D]]
  (underlying : Coll, keys : Seq[A])
  extends MutableDomainMapSliceSeq.FromKeySeq[A,D,Double,Coll](underlying, keys)
  with SliceSeq[A,D,Coll];
}
