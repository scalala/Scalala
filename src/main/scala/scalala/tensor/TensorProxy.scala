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

import domain.IterableDomain;
import generic.TensorBuilder;

import scalala.scalar.Scalar;
import scalala.generic.collection._;

/**
 * Proxy for a Tensor instance.
 *
 * @author dramage
 */
trait TensorProxyLike
[@specialized(Int, Long) K,
 @specialized(Int, Long, Float, Double, Boolean) V,
 +D<:IterableDomain[K],
 Inner <: Tensor[K,V],
 +This<:Tensor[K,V]]
extends TensorLike[K,V,D,This] {
  def inner : Inner;
  
  override def repr = inner.asInstanceOf[This];

  override def domain = inner.domain.asInstanceOf[D];
  
  override val scalar = inner.scalar;

  override def size = inner.size;
  
  override def nonzeroSize = inner.nonzeroSize;

  override def newBuilder[NK,NV:Scalar](domain : IterableDomain[NK])
  : TensorBuilder[NK,NV,Tensor[NK,NV]] =
    inner.newBuilder[NK,NV](domain);

  override def foreachPair[U](fn: (K,V) => U) : Unit =
    inner.foreachPair(fn);

  override def foreachKey[U](fn : (K=>U)) =
    inner.foreachKey(fn);
    
  override def foreachValue[U](fn : (V=>U)) =
    inner.foreachValue(fn);

  override def foreachNonZeroPair[U](fn : ((K,V)=>U)) : Boolean =
    inner.foreachNonZeroPair(fn);

  override def foreachNonZeroValue[U](fn : (V=>U)) =
    inner.foreachNonZeroValue(fn);
    
  override def forallPairs(fn : (K,V) => Boolean) : Boolean =
    inner.forallPairs(fn);

  override def forallNonZeroPairs(fn : (K,V) => Boolean) : Boolean =
    inner.forallNonZeroPairs(fn);

  override def forallValues(fn : V => Boolean) : Boolean =
    inner.forallValues(fn);
 
  override def forallNonZeroValues(fn : V => Boolean) : Boolean =
    inner.forallNonZeroValues(fn);

  override def pairsIterator : Iterator[(K,V)] =
    inner.pairsIterator;

  override def keysIterator : Iterator[K] =
    inner.keysIterator;

  override def valuesIterator : Iterator[V] =
    inner.valuesIterator;

  override def pairsIteratorNonZero : Iterator[(K,V)] =
    inner.pairsIteratorNonZero;

  override def keysIteratorNonZero : Iterator[K] =
    inner.keysIteratorNonZero;

  override def valuesIteratorNonZero : Iterator[V] =
    inner.valuesIteratorNonZero;

  override def find(p : V => Boolean) : Option[K] =
    inner.find(p);

  override def findAll(p : V => Boolean) : Iterator[K] =
    inner.findAll(p);

  override def apply(key : K) : V =
    inner(key);

  override def argsort(implicit ord : Ordering[V]) : List[K] =
    inner.argsort;

  override def argmax : K =
    inner.argmax;

  override def argmin : K =
    inner.argmin;

  override def max : V =
    inner.max;

  override def min : V =
    inner.min;

  override def sum : V =
    inner.sum;

  override def asOrdering(implicit ord : Ordering[V]) =
    inner.asOrdering;

  override def asMap =
    inner.asMap;

  override def toMap =
    inner.toMap;

  override def hashCode() =
    inner.hashCode;
}

/**
 * Proxy for a generic Tensor.
 *
 * @author dramage
 */
trait TensorProxy[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V, Inner <: Tensor[K,V]]
extends Tensor[K,V] with TensorProxyLike[K,V,IterableDomain[K],Inner,TensorProxy[K,V,Inner]];

