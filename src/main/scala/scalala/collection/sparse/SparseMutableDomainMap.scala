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
package sparse;

import domain.{DomainLike,IterableDomain};

/**
 * Implementation trait for a MutableDomainMap backed by a dense array of values.
 *
 * @author dramage
 */
trait SparseMutableDomainMapLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 D<:IterableDomain[A] with DomainLike[A,D],
 +This<:SparseMutableDomainMap[A,B,D]]
extends MutableDomainMapLike[A,B,D,This] {
  def data : SparseArray[B];

  var default : B;

  /** Assigns the given value to all elements of this map. */
  override def :=(value : B) = {
    default = value;
    data.clear();
  }
}

/**
 * MutableDomainMap backed by a dense array of values.
 *
 * @author dramage
 */
trait SparseMutableDomainMap
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 D<:IterableDomain[A] with DomainLike[A,D]]
extends MutableDomainMap[A,B,D]
with SparseMutableDomainMapLike[A,B,D,SparseMutableDomainMap[A,B,D]];
