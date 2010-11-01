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

/**
 * Proxy for a mutable tensor.
 *
 * @author dramage
 */
trait TensorProxyLike
[@specialized(Int, Long) K,
 @specialized(Int, Long, Float, Double, Boolean) V,
 +D<:IterableDomain[K] with DomainLike[K,D],
 Inner <: Tensor[K,V],
 +This <: Tensor[K,V]]
extends tensor.TensorProxyLike[K,V,D,Inner,This] with TensorLike[K,V,D,This] {

  override def update(key : K, value : V) : Unit =
    inner.update(key, value);

  override def transform(f : (K,V)=>V) =
    inner.transform(f);

  override def transformNonZero(fn : ((K,V)=>V)) : Boolean =
    inner.transformNonZero(fn);

  override def transformValues(f : V=>V) =
    inner.transformValues(f);

  override def transformNonZeroValues(fn : (V=>V)) =
    inner.transformNonZeroValues(fn);
}

/**
 * A proxy for a mutable Tensor.
 *
 * @author dramage
 */
trait TensorProxy[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B, Inner <: Tensor[A,B]]
extends tensor.TensorProxy[A,B,Inner] with Tensor[A,B] with TensorProxyLike[A,B,IterableDomain[A],Inner,TensorProxy[A,B,Inner]];

/**
 * A proxy for a mutable Tensor1.
 * 
 * @author dramage
 */
trait Tensor1ProxyLike
[@specialized(Int, Long) K,
 @specialized(Int, Long, Float, Double, Boolean) V,
 +D<:IterableDomain[K] with DomainLike[K,D],
 Inner <: Tensor1[K,V],
 +This <: Tensor1[K,V]]
extends tensor.Tensor1ProxyLike[K,V,D,Inner,This]
   with TensorProxyLike[K,V,D,Inner,This] with Tensor1Like[K,V,D,This];

/**
 * A proxy for a mutable Tensor1.
 *
 * @author dramage
 */
trait Tensor1Proxy
[@specialized(Int, Long) K,
 @specialized(Int, Long, Float, Double, Boolean) V,
 Inner <: Tensor1[K,V]]
extends tensor.Tensor1Proxy[K,V,Inner]
   with TensorProxy[K,V,Inner] with Tensor1[K,V]
   with Tensor1ProxyLike[K,V,IterableDomain[K],Inner,Tensor1Proxy[K,V,Inner]];

/**
 * A proxy for a mutable Vector.
 *
 * @author dramage
 */
trait VectorProxyLike[@specialized(Int,Long,Float,Double) V, Inner<:Vector[V], +This<:Vector[V]]
extends tensor.VectorProxyLike[V,Inner,This] with Tensor1ProxyLike[Int,V,IndexDomain,Inner,This] with VectorLike[V,This];

/**
 * A proxy for a mutable vector.
 *
 * @author dramage
 */
trait VectorProxy[@specialized(Int,Long,Float,Double) V, Inner<:Vector[V]]
extends tensor.VectorProxy[V,Inner] with Tensor1Proxy[Int,V,Inner] with Vector[V] with VectorProxyLike[V,Inner,VectorProxy[V,Inner]];
