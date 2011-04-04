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
import generic.{TensorBuilder,TensorNomadic};

import scalala.scalar.Scalar;
import scalala.generic.collection._;

/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dramage
 */
trait CounterLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +M<:scala.collection.Map[K,V],
 +This<:Counter[K,V]]
extends TensorLike[K,V,SetDomain[K],This]
with TensorNomadic[K,V,This] { self =>

  def data : M;
  
  override def domain : SetDomain[K] = new SetDomain(data.keySet);
  
  override def apply(k : K) = data(k);

  //
  // non-tupled monadic
  //

  def foreach[U](fn : (K,V)=>U) : Unit =
    foreachPair(fn);

  def foreachNonZero[U](fn : (K,V)=>U) : Unit =
    foreachNonZeroPair(fn);
  
  def map[TT>:This,RV,That](fn : (K,V)=>RV)
  (implicit bf : CanMapKeyValuePairs[TT, K, V, RV, That]) : That =
    mapPairs[TT,RV,That](fn)(bf);
}

trait Counter
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor[K,V] with CounterLike[K,V,scala.collection.Map[K,V],Counter[K,V]];

object Counter {
  def apply[K,V:Scalar](values : (K,V)*) : mutable.Counter[K,V] =
    mutable.Counter(values :_ *);
}

