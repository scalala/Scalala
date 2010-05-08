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
package scalala.library;

import scalala.collection.PartialMap;

/**
 * Math on PartialMap instances.
 *
 * @author dramage
 */
trait PartialMaps extends Traversables with Iterators {

  /** Maximum value of the map. */
  def max[I](v : PartialMap[I,Double]) : Double = {
    val nonActive = v.domain -- v.activeDomain;
    if (nonActive.isEmpty) {
      max(v.activeValues);
    } else {
      max(Iterator(v.default) ++ v.activeValues);
    }
  }

  /** Maximum value of the map. */
  def max[I,V](v : PartialMap[I,V])(implicit cv : V=>Double) : Double =
    max(v.map(cv));

  /** Returns a key i of the map such that v(i)=max(v). */
  def argmax[I](v : PartialMap[I,Double]) : I = {
    val nonActive = v.domain -- v.activeDomain;
    if (nonActive.isEmpty) {
      argmax(v.activeElements);
    } else {
      argmax(Iterator((nonActive.iterator.next, v.default)) ++ v.activeElements);
    }
  }

  def argmax[I,V](v : PartialMap[I,V])(implicit cv : V=>Double) : I =
    argmax(v.map(cv));

  /** The minimum value of the map. */
  def min[I](v : PartialMap[I,Double]) : Double = {
    val nonActive = v.domain -- v.activeDomain;
    if (nonActive.isEmpty) {
      min(v.activeValues);
    } else {
      min(Iterator(v.default) ++ v.activeValues);
    }
  }

  /** Maximum value of the map. */
  def min[I,V](v : PartialMap[I,V])(implicit cv : V=>Double) : Double =
    min(v.map(cv));

  /** Returns a key i of the map such that v(i)=min(v). */
  def argmin[I](v : PartialMap[I,Double]) : I = {
    val nonActive = v.domain -- v.activeDomain;
    if (nonActive.isEmpty) {
      argmin(v.activeElements);
    } else {
      argmin(Iterator((nonActive.iterator.next, v.default)) ++ v.activeElements);
    }
  }

  def argmin[I,V](v : PartialMap[I,V])(implicit cv : V=>Double) : I =
    argmin(v.map(cv));

}