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
 * Implementation trait for a transposed view of an underlying DomainMap2.
 *
 * @author dramage
 */
trait DomainMap2TransposeLike
[@specialized A2, @specialized A1, @specialized B,
 D2<:IterableDomain[A2],D1<:IterableDomain[A1],
 ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
 +Coll <: DomainMap2[A1,A2,B,D1,D2,ID],
 +This <: DomainMap2Transpose[A2,A1,B,D2,D1,ID,OD,Coll]]
extends DomainMapSliceLike[(A1,A2),ID,(A2,A1),OD,B,Coll,This]
with DomainMap2Like[A2,A1,B,D2,D1,OD,This] {
self =>

  final override def lookup(tup : (A2,A1)) = tup.swap;

  override def apply(i : A2, j : A1) = underlying.apply(j, i);

  def transpose = underlying;
}

/**
 * Transposed view of an underlying DomainMap2.
 */
trait DomainMap2Transpose
[@specialized A2, @specialized A1, @specialized B,
 D2<:IterableDomain[A2],D1<:IterableDomain[A1],
 ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
 +Coll <: DomainMap2[A1,A2,B,D1,D2,ID]]
extends DomainMapSlice[(A1,A2),ID,(A2,A1),OD,B,Coll]
with DomainMap2[A2,A1,B,D2,D1,OD]
with DomainMap2TransposeLike[A2,A1,B,D2,D1,ID,OD,Coll,DomainMap2Transpose[A2,A1,B,D2,D1,ID,OD,Coll]];


object DomainMap2Transpose {
  /** Default implementation. */
  class Impl
  [@specialized A2, @specialized A1, @specialized B,
   D2<:IterableDomain[A2], D1<:IterableDomain[A1],
   ID<:Product2Domain[A1,A2,D1,D2], OD<:Product2Domain[A2,A1,D2,D1],
   +Coll <: DomainMap2[A1,A2,B,D1,D2,ID]]
  (override val underlying : Coll, override val domain : OD)
  extends DomainMap2Transpose[A2,A1,B,D2,D1,ID,OD,Coll] {
    override def copy = new Impl(underlying.copy, domain.copy);
  }
}
