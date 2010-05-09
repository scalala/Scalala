/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package collection.dense

import collection.{MutableDomainMapLike,MutableDomainMap};
import collection.domain.IterableDomain;

/**
 * Implementation trait for a MutableDomainMap backed by a dense array of values.
 *
 * @author dramage
 */
trait DenseMutableDomainMapLike
[@specialized A, @specialized B, D<:IterableDomain[A], +This]
extends MutableDomainMapLike[A,B,D,This] {
  def data : Array[B];

  /** Assigns the given value to all elements of this map. */
  override def :=(value : B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = value;
      i += 1;
    }
  }

  /** Tranforms all values in this map by applying the given function. */
  override def transformValues(f : B=>B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(data(i));
      i += 1;
    }
  }
}

/**
 * MutableDomainMap backed by a dense array of values.
 *
 * @author dramage
 */
trait DenseMutableDomainMap[@specialized A, @specialized B, D<:IterableDomain[A]]
extends MutableDomainMap[A,B,D]
with DenseMutableDomainMapLike[A,B,D,DenseMutableDomainMap[A,B,D]];
