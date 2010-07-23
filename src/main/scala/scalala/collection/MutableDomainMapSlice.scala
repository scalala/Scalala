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
 * Implementation trait for slices of an underlying MutableDomainMap.  A slice
 * is a pass-through view of a (mapped) subset of the original DomainMap's
 * domain.
 *
 * @author dramage
 */
trait MutableDomainMapSliceLike
[@specialized(Int,Long) A1, D1<:IterableDomain[A1] with DomainLike[A1,D1],
 @specialized(Int,Long) A2, D2<:IterableDomain[A2] with DomainLike[A2,D2],
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll <: MutableDomainMap[A1, B, D1],
 +This <: MutableDomainMapSlice[A1, D1, A2, D2, B, Coll]]
extends MutableDomainMapLike[A2,B,D2,This]
with DomainMapSliceLike[A1,D1,A2,D2,B,Coll,This] {

  override def update(key : A2, value : B) =
    underlying.update(lookup(key), value);
}

/**
 * Pass-through view of a (key-mapped) subset of an underlying MutableDomainMap.
 *
 * @author dramage
 */
trait MutableDomainMapSlice
[@specialized(Int,Long) A1, D1<:IterableDomain[A1] with DomainLike[A1,D1],
 @specialized(Int,Long) A2, D2<:IterableDomain[A2] with DomainLike[A2,D2],
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll <: MutableDomainMap[A1, B, D1]]
extends MutableDomainMap[A2,B,D2] with DomainMapSlice[A1,D1,A2,D2,B,Coll]
with MutableDomainMapSliceLike[A1, D1, A2, D2, B, Coll, MutableDomainMapSlice[A1, D1, A2, D2, B, Coll]];


object MutableDomainMapSlice {
  class FromKeyMap
  [@specialized(Int,Long) A1, D1<:IterableDomain[A1] with DomainLike[A1,D1],
   @specialized(Int,Long) A2, @specialized(Int,Long,Float,Double,Boolean) B,
   +Coll<:MutableDomainMap[A1, B, D1]]
  (underlying : Coll, keymap : scala.collection.Map[A2,A1])
  extends DomainMapSlice.FromKeyMap[A1,D1,A2,B,Coll](underlying, keymap)
  with MutableDomainMapSlice[A1,D1,A2,SetDomain[A2],B,Coll];
}
