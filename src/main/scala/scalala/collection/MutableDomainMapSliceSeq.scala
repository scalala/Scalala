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
package collection;

import domain._;
import generic._;

/**
 * Implementation trait for a pass-through DomainSeq-like view of an underlying
 * MutableDomainMap.
 *
 * @author dramage
 */
trait MutableDomainMapSliceSeqLike
[@specialized(Int,Long) A1, +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:MutableDomainMap[A1,B],
 +This<:MutableDomainMapSliceSeq[A1,B,Coll]]
extends MutableDomainMapSliceLike[A1, D1, Int, IndexDomain, B, Coll, This]
with DomainMapSliceSeqLike[A1, D1, B, Coll, This]
with MutableDomainSeqLike[B, This];

/**
 * DomainSeq like view of an underlying MutableDomainMap.
 */
trait MutableDomainMapSliceSeq
[@specialized(Int,Long) A1, @specialized(Int,Long,Float,Double,Boolean) B,
 +Coll<:MutableDomainMap[A1, B]]
extends MutableDomainMapSlice[A1,Int,B,Coll]
with DomainMapSliceSeq[A1,B,Coll] with MutableDomainSeq[B]
with MutableDomainMapSliceSeqLike[A1, IterableDomain[A1], B, Coll, MutableDomainMapSliceSeq[A1, B, Coll]];

object MutableDomainMapSliceSeq {
  class FromKeySeq[A1, B, +Coll<:MutableDomainMap[A1, B]]
  (underlying : Coll, keys : Seq[A1])
  extends DomainMapSliceSeq.FromKeySeq[A1,B,Coll](underlying, keys)
  with MutableDomainMapSliceSeq[A1,B,Coll];
}
