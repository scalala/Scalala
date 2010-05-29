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
 * Implementation trait for a transposed view of an underlying MutableDomainMap2.
 *
 * @author dramage
 */
trait MutableDomainMap2TransposeLike
[@specialized A2, @specialized A1, @specialized B,
 D2<:IterableDomain[A2], D1<:IterableDomain[A1],
 ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
 +Coll <: MutableDomainMap2[A1,A2,B,D1,D2,ID],
 +This <: MutableDomainMap2Transpose[A2,A1,B,D2,D1,ID,OD,Coll]]
extends DomainMap2TransposeLike[A2,A1,B,D2,D1,ID,OD,Coll,This]
with MutableDomainMapSliceLike[(A1,A2),ID,(A2,A1),OD,B,Coll,This]
with MutableDomainMap2Like[A2,A1,B,D2,D1,OD,This] {
self =>

  override def update(i : A2, j : A1, value : B) =
    underlying.update(j, i, value);
}

/**
 * Transposed view of an undelrying MutableDomainMap2.
 *
 * @author dramage
 */
trait MutableDomainMap2Transpose
[@specialized A2, @specialized A1, @specialized B,
 D2<:IterableDomain[A2],D1<:IterableDomain[A1],
 ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
 +Coll <: MutableDomainMap2[A1,A2,B,D1,D2,ID]]
extends DomainMap2Transpose[A2,A1,B,D2,D1,ID,OD,Coll]
with MutableDomainMapSlice[(A1,A2),ID,(A2,A1),OD,B,Coll]
with MutableDomainMap2[A2,A1,B,D2,D1,OD]
with MutableDomainMap2TransposeLike[A2,A1,B,D2,D1,ID,OD,Coll,MutableDomainMap2Transpose[A2,A1,B,D2,D1,ID,OD,Coll]];


object MutableDomainMap2Transpose {
  /** Default implementation. */
  class Impl
  [@specialized A2, @specialized A1, @specialized B,
   D2<:IterableDomain[A2], D1<:IterableDomain[A1],
   ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
   +Coll <: MutableDomainMap2[A1,A2,B,D1,D2,ID]]
  (override val underlying : Coll, override val domain : OD)
  extends MutableDomainMap2Transpose[A2,A1,B,D2,D1,ID,OD,Coll];
}
