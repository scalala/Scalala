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
package numeric;

import domain._;

/**
 * A Mutable DomainMap with numeric values.
 *
 * @author dramage
 */
trait NumericDomainMapLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B,
 D<:IterableDomain[A] with DomainLike[A,D],
 +This<:NumericDomainMap[A,B,D]]
extends DomainMapLike[A,B,D,This] {

  val numeric : Numeric[B];

  //
  // Collection level queries
  //

  /** Returns a key associated with the largest value in the map. */
  def argmax : A = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .max");
    }
    var max = valuesIterator.next;
    var arg = keysIterator.next
    foreach((k,v) => if (numeric.>(v, max)) { max = v; arg = k; });
    arg;
  }

  /** Returns a key associated with the smallest value in the map. */
  def argmin : A = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .min");
    }
    var min = valuesIterator.next;
    var arg = keysIterator.next
    foreach((k,v) => if (numeric.<(v,min)) { min = v; arg = k; });
    arg;
  }

  /** Returns the max of the values in this map. */
  def max : B = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .max");
    }
    var max = valuesIterator.next;
    valuesIterator.foreach(v => { if (numeric.>(v,max)) max = v; })
    return max;
  }

  /** Returns the min of the values in this map. */
  def min : B = {
    if (!valuesIterator.hasNext) {
      throw new UnsupportedOperationException("Empty .min");
    }
    var min = valuesIterator.next;
    valuesIterator.foreach(v => { if (numeric.<(v,min)) min = v; })
    return min;
  }
}

/**
 * A Mutable DomainMap with numeric values.
 *
 * @author dramage
 */
trait NumericDomainMap
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B,
 D<:IterableDomain[A] with DomainLike[A,D]]
extends DomainMap[A,B,D] with NumericDomainMapLike[A,B,D,NumericDomainMap[A,B,D]]
