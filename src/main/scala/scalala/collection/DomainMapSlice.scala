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
 * Implementation trait for slices of an underlying DomainMap.  A slice
 * is a pass-through view of a (mapped) subset of the original DomainMap's
 * domain.
 *
 * @author dramage
 */
trait DomainMapSliceLike
[@specialized(Int,Long) A1, +D1<:IterableDomain[A1] with DomainLike[A1,D1],
 @specialized(Int,Long) A2, +D2<:IterableDomain[A2] with DomainLike[A2,D2],
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll<:DomainMap[A1,B],
 +This<:DomainMapSlice[A1,A2,B,Coll]]
extends DomainMapLike[A2,B,D2,This] {
self =>

  /** The collection underlying this view. */
  protected def underlying: Coll;

  /** Maps the keys of this domain map to the keys of the underlying maps's. */
  def lookup(key : A2) : A1;

  override def apply(key : A2) =
    underlying.apply(lookup(key));
}

/**
 * Pass-through view of a (key-mapped) subset of an underlying DomainMap.
 *
 * @author dramage
 */
trait DomainMapSlice
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B, +Coll <: DomainMap[A1, B]]
extends DomainMap[A2,B]
with DomainMapSliceLike[A1, IterableDomain[A1], A2, IterableDomain[A2], B, Coll, DomainMapSlice[A1, A2, B, Coll]];

object DomainMapSlice {
  class FromKeyMap[A1, A2, B, +Coll<:DomainMap[A1, B]]
  (override val underlying : Coll, keymap : scala.collection.Map[A2,A1])
  extends DomainMapSlice[A1, A2, B, Coll] {
    override def lookup(key : A2) = keymap(key);
    override val domain = new SetDomain(keymap.keySet);
  }
}
