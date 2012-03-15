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

import scalala.scalar.Scalar
import scalala.generic.collection._
;


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
extends Tensor1Like[K,V,SetDomain[K],This] { self =>

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
  
  override def size = data.size;

  def isEmpty = data.isEmpty

  def contains(k: K) = data.contains(k);

  override def apply(k : K) = {
    data.get(k) getOrElse scalar.zero
  };

  def get(k: K) = data.get(k);

  override def checkKey(k : K) = ();
  
  override def checkDomain(d : scalala.tensor.domain.Domain[K]) = ();
  
  //
  // faster implementations
  //
  
  override def foreachPair[U](fn : (K,V) => U) : Unit =
    data.foreach(fn.tupled);
  
  override def foreachKey[U](fn : K => U) : Unit =
    data.keysIterator.foreach(fn);
  
  override def foreachValue[U](fn : V => U) : Unit =
    data.valuesIterator.foreach(fn);
    
  override def keysIterator = data.keysIterator;
  
  override def valuesIterator = data.valuesIterator;
  
  override def pairsIterator = data.iterator;
}

trait Counter
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor1[K,V] with CounterLike[K,V,scala.collection.Map[K,V],Counter[K,V]];

private[scalala]
trait CounterRowProxyLike[K,V,+This<:CounterRowProxy[K,V]]
extends Tensor1ProxyLike[K,V,Domain1[K],Counter[K,V],This]
with Tensor1RowLike[K,V,Domain1[K],This];

private[scalala]
trait CounterRowProxy[K,V]
extends Tensor1Proxy[K,V,Counter[K,V]] with Tensor1Row[K,V]
with CounterRowProxyLike[K,V,CounterRowProxy[K,V]];

private[scalala]
trait CounterColProxyLike[K,V,+This<:CounterColProxy[K,V]]
extends Tensor1ProxyLike[K,V,Domain1[K],Counter[K,V],This]
with Tensor1ColLike[K,V,Domain1[K],This];

private[scalala]
trait CounterColProxy[K,V]
extends Tensor1Proxy[K,V,Counter[K,V]] with Tensor1Col[K,V]
with CounterColProxyLike[K,V,CounterColProxy[K,V]];

trait CounterImplicitsLevel0 {
  /** View a Counter as a row. */
  implicit def counterAsRow[K,V](counter : Counter[K,V]) : Tensor1Row[K,V]
  = new CounterRowProxy[K,V] { override def inner = counter; }
}

trait CounterImplicitsLevel1 extends CounterImplicitsLevel0 {
  /** View a Counter as a column. */
  implicit def counterAsCol[K,V](counter : Counter[K,V]) : Tensor1Col[K,V]
  = new CounterColProxy[K,V] { override def inner = counter; }
}

object Counter extends CounterImplicitsLevel1 {
  def apply[K,V:Scalar]() : mutable.Counter[K,V] =
    mutable.Counter();
  
  def apply[K,V:Scalar](values : (K,V)*) : mutable.Counter[K,V] =
    mutable.Counter(values :_ *);
  
  def apply[K,V:Scalar](values : TraversableOnce[(K,V)]) : mutable.Counter[K,V] =
    mutable.Counter(values);
    
  def apply[K,V:Scalar](domain : Domain1[K]) =
    mutable.Counter(domain);
  
  def count[K](items : TraversableOnce[K]) : mutable.Counter[K,Int] =
    mutable.Counter.count(items);

  def count[K](items: K*): mutable.Counter[K,Int] = count(items);

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

