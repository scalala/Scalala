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
import generic.{TensorBuilder,TensorPairsMonadic};

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
extends Tensor1Like[K,V,SetDomain[K],This]
with TensorPairsMonadic[K,V,This] { self =>

  override def newBuilder[NK,NV:Scalar](domain : IterableDomain[NK])
  : TensorBuilder[NK,NV,Tensor[NK,NV]] = domain match {
    case that : IndexDomain =>
      super.newBuilder(that)
    case that : Domain1[_] =>
      mutable.Counter(that)(implicitly[Scalar[NV]]).asBuilder;
    case _ =>
      super.newBuilder(domain);
  }

  def data : M;
  
  override def domain : SetDomain[K] = new SetDomain(data.keySet);
  
  override def apply(k : K) = data.getOrElse(k,scalar.zero);

  override def checkKey(k : K) = ();
  
  override def checkDomain(d : scalala.tensor.domain.Domain[K]) = ();

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
    
  //
  // faster implementations
  //
  
  override def foreachPair[U](fn : (K,V) => U) : Unit =
    data.foreach(fn.tupled);
  
  override def foreachKey[U](fn : K => U) : Unit =
    data.keys.foreach(fn);
  
  override def foreachValue[U](fn : V => U) : Unit =
    data.values.foreach(fn);
    
  override def keysIterator = data.keysIterator;
  
  override def valuesIterator = data.valuesIterator;
  
  override def pairsIterator = data.iterator;
}

trait Counter
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor1[K,V] with CounterLike[K,V,scala.collection.Map[K,V],Counter[K,V]];

object Counter {
  def apply[K,V:Scalar](values : (K,V)*) : mutable.Counter[K,V] =
    mutable.Counter(values :_ *);
    
  def apply[K,V:Scalar](domain : Domain1[K]) =
    mutable.Counter(domain);
}

