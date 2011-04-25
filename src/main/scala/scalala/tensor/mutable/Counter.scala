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
package mutable;

import domain._;

import scalala.scalar.Scalar;
import scalala.generic.collection._;

/**
 * A mutable tensor that acts like a collection of key-value pairs backed by
 * a map.
 *
 * @author dramage
 */
trait CounterLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +M<:scala.collection.mutable.Map[K,V],
 +This<:Counter[K,V]]
extends tensor.CounterLike[K,V,M,This] with Tensor1Like[K,V,SetDomain[K],This] { self =>

  def update(k : K, v : V) =
    data(k) = v;
}

trait Counter
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends tensor.Counter[K,V] with Tensor1[K,V]
with CounterLike[K,V,scala.collection.mutable.Map[K,V],Counter[K,V]];

object Counter {
  class Impl[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
  (override val data : scala.collection.mutable.Map[K,V])
  (implicit override val scalar : Scalar[V])
  extends Counter[K,V];

  /** Returns an empty counter. */
  def apply[K,V:Scalar]() : Counter[K,V] =
    new Impl(scala.collection.mutable.HashMap[K,V]());

  /** Returns a counter by summing all the given values. */
  def apply[K,V:Scalar](values : (K,V)*) : Counter[K,V] =
    apply(values);
  
  /** Returns a counter by summing all the given values. */
  def apply[K,V:Scalar](values : TraversableOnce[(K,V)]) : Counter[K,V] = {
    val rv = apply[K,V]();
    values.foreach({ case (k,v) => rv(k) = rv.scalar.+(v,rv(k)); });
    rv;
  }

  /** Counts each of the given items. */
  def count[K](items : TraversableOnce[K]) : Counter[K,Int] = {
    val rv = apply[K,Int]();
    items.foreach(rv(_) += 1);
    rv;
  }
  
  def apply[K,V:Scalar](domain : Domain1[K]) : Counter[K,V] =
    new Impl(scala.collection.mutable.HashMap[K,V]());

//  implicit def opCanCopy[K,V:CanCopy] = CanCopy.opMapValues[Counter[K,V],V];
//  implicit def opZeros[K,V:Scalar] = CanCreateZerosLike.opMapValues[Counter[K,V],V,Counter[K,V]];

  implicit def CanMapValuesCounter
  [@specialized(Int) K, @specialized(Int,Double) V, @specialized(Int,Double) RV:Scalar]: CanMapValues[Counter[K, V], V, RV, Counter[K, RV]]
  = new CanMapValues[Counter[K,V],V,RV,Counter[K,RV]] {
    override def map(from : Counter[K,V], fn : (V=>RV)) = {
      val rv = Counter[K,RV]();
      for( (k,v) <- from.pairsIterator) {
        rv(k) = fn(from.data(k));
      }
      rv;
    }

    override def mapNonZero(from : Counter[K,V], fn : (V=>RV)) = {
      val rv = Counter[K,RV]();
      for( (k,v) <- from.pairsIteratorNonZero) {
        rv(k) = fn(from.data(k));
      }
      rv;
    }
  }
}

