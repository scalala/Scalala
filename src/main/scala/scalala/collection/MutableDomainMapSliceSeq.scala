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
[@specialized A1, D1<:IterableDomain[A1],
 @specialized B, +Coll<:MutableDomainMap[A1, B, D1],
 +This <: MutableDomainMapSliceSeqLike[A1, D1, B, Coll, This]]
extends MutableDomainMapSliceLike[A1, D1, Int, IndexDomain, B, Coll, This]
with DomainMapSliceSeqLike[A1, D1, B, Coll, This]
with MutableDomainSeqLike[B, This];

/**
 * DomainSeq like view of an underlying MutableDomainMap.
 */
trait MutableDomainMapSliceSeq
[@specialized A1, D1<:IterableDomain[A1],
 @specialized B, +Coll<:MutableDomainMap[A1, B, D1]]
extends MutableDomainSeq[B]
with DomainMapSliceSeq[A1,D1,B,Coll]
with MutableDomainMapSliceSeqLike[A1, D1, B, Coll, MutableDomainMapSliceSeq[A1, D1, B, Coll]];

object MutableDomainMapSliceSeq {
  class FromKeySeq
  [@specialized A1, D1<:IterableDomain[A1], @specialized B, +Coll<:MutableDomainMap[A1, B, D1]]
  (underlying : Coll, keys : Seq[A1])
  extends DomainMapSliceSeq.FromKeySeq[A1,D1,B,Coll](underlying, keys)
  with MutableDomainMapSliceSeq[A1,D1,B,Coll];
}
