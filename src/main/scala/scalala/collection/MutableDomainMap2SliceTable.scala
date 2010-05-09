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
 * Implementation trait for a pass-through view of a (key-mapped)
 * MutableDomainTable from an underlying MutableDomainMap2.
 *
 * @author dramage
 */
trait MutableDomainMap2SliceTableLike
[@specialized A1, @specialized A2, @specialized B,
 D1<:IterableDomain[A1], D2<:IterableDomain[A2],
 D<:Product2Domain[A1,A2,D1,D2],
 +Coll<:MutableDomainMap2[A1,A2,B,D1,D2,D],
 +This<:MutableDomainMap2SliceTableLike[A1,A2,B,D1,D2,D,Coll,This]]
extends DomainMap2SliceTableLike[A1,A2,B,D1,D2,D,Coll,This]
with MutableDomainTableLike[B,This] {

  override def update(i : Int, j : Int, value : B) : Unit =
    underlying.update(lookup1(i),lookup2(j),value);
}

/**
 * A pass-through view of a (key-mapped)
 * MutableDomainTable from an underlying MutableDomainMap2.
 *
 * @author dramage
 */
trait MutableDomainMap2SliceTable
[@specialized A1, @specialized A2, @specialized B,
 D1<:IterableDomain[A1], D2<:IterableDomain[A2],
 D<:Product2Domain[A1,A2,D1,D2],
 +Coll <: MutableDomainMap2[A1,A2,B,D1,D2,D]]
extends MutableDomainTable[B]
with DomainMap2SliceTable[A1,A2,B,D1,D2,D,Coll]
with MutableDomainMap2SliceTableLike[A1,A2,B,D1,D2,D,Coll,MutableDomainMap2SliceTable[A1,A2,B,D1,D2,D,Coll]];

object MutableDomainMap2SliceTable {
  class FromKeySeqs
  [@specialized A1, @specialized A2, @specialized B,
   D1<:IterableDomain[A1], D2<:IterableDomain[A2],
   D<:Product2Domain[A1,A2,D1,D2],
   +Coll <: MutableDomainMap2[A1,A2,B,D1,D2,D]]
  (override val underlying : Coll, val keys1 : Seq[A1], val keys2 : Seq[A2])
  extends MutableDomainMap2SliceTable[A1,A2,B,D1,D2,D,Coll] {
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }
}
