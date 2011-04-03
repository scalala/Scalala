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
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +M<:scala.collection.Map[K,V],
 +This<:Counter[K,V]]
extends TensorLike[K,V,SetDomain[K],This] { self =>

  def data : M;
  
  override def domain : SetDomain[K] = new SetDomain(data.keySet);
  
  override def apply(k : K) = data(k);

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
  def foreach[U](fn : ((K,V)) => U) : Unit =
    this.foreachPair((k,v) => fn((k,v)));

  /**
   * For-comprehension support for "for ((k,v) <- x) yield ..."
   * Defers to un-tupled map.
   */
  def map[TT>:This, RV, That](f: ((K,V)) => RV)
  (implicit map: CanMapKeyValuePairs[TT, K, V, RV, That]): That =
    this.mapPairs[TT,RV,That]((k : K, v : V) => f((k,v)));
  
  def filter(f : ((K,V)) => Boolean) =
    new Counter.Filtered[K,V,This](repr, f);
  
  def withFilter(f : ((K,V)) => Boolean) =
    new Counter.Filtered[K,V,This](repr, f);

}

trait Counter
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor[K,V] with CounterLike[K,V,scala.collection.Map[K,V],Counter[K,V]];

object Counter {
  def apply[K,V:Scalar](values : (K,V)*) : mutable.Counter[K,V] =
    mutable.Counter(values :_ *);

  class Filtered[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V, +This<:Counter[K,V]]
  (inner : This, p : ((K,V)) => Boolean) {
    def foreach[U](q : ((K,V)) => U) =
      inner.foreach(tup => if (p(tup)) q(tup));
  
    def withFilter(q : ((K,V)) => Boolean) =
      new Filtered[K,V,This](inner, tup => p(tup) && q(tup));
    
    def map[U,D,That](fn : ((K,V)) => U)
    (implicit df : CanGetDomain[This,D], bf : CanBuildTensorFrom[This,D,K,U,That]) = {
      val builder = bf(inner, inner.domain.asInstanceOf[D]);
      inner.foreach(tup => if (p(tup)) builder(tup._1) = fn(tup));
      builder.result;
    }
  }
}

