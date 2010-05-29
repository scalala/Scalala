
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
 * Implementation trait for a DomainSeqLike view of a slice of keys from a DomainMap.
 *
 * @author dramage
 */
trait DomainMapSliceSeqLike
[@specialized A, D<:IterableDomain[A],
 @specialized B, +Coll<:DomainMap[A, B, D],
 +This <: DomainMapSliceSeq[A, D, B, Coll]]
extends DomainMapSliceLike[A, D, Int, IndexDomain, B, Coll, This]
with DomainSeqLike[B, This];

/**
 * A DomainSeqLike view of a slice of keys from a DomainMap.
 *
 * @author dramage
 */
trait DomainMapSliceSeq
[@specialized A, D<:IterableDomain[A],
 @specialized B, +Coll<:DomainMap[A, B, D]]
extends DomainMapSlice[A,D,Int,IndexDomain,B,Coll] with DomainSeq[B]
with DomainMapSliceSeqLike[A, D, B, Coll, DomainMapSliceSeq[A, D, B, Coll]];

object DomainMapSliceSeq {
  class FromKeySeq
  [@specialized A, D<:IterableDomain[A], @specialized B, +Coll <: DomainMap[A, B, D]]
  (override val underlying : Coll, keys : Seq[A])
  extends DomainMapSliceSeq[A, D, B, Coll] {
    override def lookup(key : Int) = keys(key);
    override val domain = IndexDomain(keys.length);
  }
}
