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

import scalar.Scalar;

import domain._;
import generic.collection._;

import mutable.TensorBuilder;

/**
 * Proxy for a Tensor instance.
 *
 * @author dramage
 */
trait TensorProxyLike
[@specialized(Int, Long) A,
 @specialized(Int, Long, Float, Double, Boolean) B,
 +D<:IterableDomain[A] with DomainLike[A,D],
 Inner <: Tensor[A,B],
 +This<:Tensor[A,B]]
extends TensorLike[A,B,D,This] {
  def inner : Inner;
  
  override def repr = inner.asInstanceOf[This];

  override def domain = inner.domain.asInstanceOf[D];
  
  override val scalar = inner.scalar;

  override def newBuilder[NK,NV:Scalar](domain : IterableDomain[NK]) : TensorBuilder[NK,NV,Tensor[NK,NV]] =
    inner.newBuilder[NK,NV](domain);

  override def foreach[U](f: ((A,B)) => U) : Unit =
    inner.foreach(f);

  override def foreach[U](fn: (A,B) => U) : Unit =
    inner.foreach(fn);

  override def foreachValue[U](fn : (B=>U)) =
    inner.foreachValue(fn);

  override def foreachNonZero[U](fn : ((A,B)=>U)) : Boolean =
    inner.foreachNonZero(fn);

  override def foreachNonZeroValue[U](fn : (B=>U)) =
    inner.foreachNonZeroValue(fn);

  override def iterator : Iterator[(A,B)] =
    inner.iterator;

  override def valuesIterator : Iterator[B] =
    inner.valuesIterator;

  override def find(p : B => Boolean) : Iterable[A] =
    inner.find(p);

  override def apply(key : A) : B =
    inner(key);

  override def argsort(implicit cm : Manifest[A], ord : Ordering[B]) : Array[A] =
    inner.argsort;

  override def argmax : A =
    inner.argmax;

  override def argmin : A =
    inner.argmin;

  override def max : B =
    inner.max;

  override def min : B =
    inner.min;

  override def sum : B =
    inner.sum;

  override def asOrdering(implicit ord : Ordering[B]) =
    inner.asOrdering;

  override def asMap =
    inner.asMap;

  override def toMap =
    inner.toMap;

  override def hashCode() =
    inner.hashCode;
}

/**
 * A proxy for a generic Tensor.
 *
 * @author dramage
 */
trait TensorProxy[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B, Inner <: Tensor[A,B]]
extends Tensor[A,B] with TensorProxyLike[A,B,IterableDomain[A],Inner,TensorProxy[A,B,Inner]];
