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
package tensor;

import domain._;
import mutable.TensorBuilder;

import scalala.scalar.Scalar;
import scalala.generic.collection._;

/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dramage
 */
trait CounterLike
[@specialized(Int,Long)K, @specialized(Int,Long,Float,Double) V,
 +D<:IterableDomain[K] with DomainLike[K,D], +This<:Counter[K,V]]
extends TensorLike[K,V,D,This] { self =>

  //
  // for-comprehensions.
  //
  // Note:
  // 1. flatMap doesn't make sense here because the value types are
  //    always scalar, so you can't do { for ((k,v) <- tensor; value <- v) yield ... }.
  //    Although I guess you might want to iterate over they key types, k,
  //    but I'm not supporting that.
  //

  /** Calls this.foreachPair(fn). */
  def foreach[U](fn : (K,V) => U) : Unit =
    this.foreachPair(fn);

  /**
   * For-comprehension support for "for ((k,v) <- x) yield ..."
   * Defers to un-tupled map.
   */
  def map[TT>:This, RV, That](f: ((K,V)) => RV)
  (implicit map: CanMapKeyValuePairs[TT, K, V, RV, That]): That =
    this.mapPairs[TT,RV,That]((k : K, v : V) => f((k,v)));

}

trait Counter
[@specialized(Int,Long)K, @specialized(Int,Long,Float,Double) V]
extends Tensor[K,V] with CounterLike[K,V,IterableDomain[K],Counter[K,V]];


