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

import domain.Domain1;

import scalala.scalar.Scalar;
import scalala.operators._;

/**
 * Implementation trait for proxies to a Tensor1 instance.
 *
 * @author dramage
 */
trait Tensor1ProxyLike
[@specialized(Int,Long)K, @specialized(Int,Long,Float,Double) V,
 +D<:Domain1[K], Inner<:Tensor1[K,V], +This<:Tensor1[K,V]]
extends TensorProxyLike[K,V,D,Inner,This] with Tensor1Like[K,V,D,This] {
  override def norm(n : Double) =
    inner.norm(n);
}

/**
 * Proxy to a Tensor1 instance.
 * 
 * @author dramage
 */
trait Tensor1Proxy
[@specialized(Int,Long)K, @specialized(Int,Long,Float,Double) V, Inner<:Tensor1[K,V]]
extends TensorProxy[K,V,Inner] with Tensor1[K,V] with Tensor1ProxyLike[K,V,Domain1[K],Inner,Tensor1Proxy[K,V,Inner]];

