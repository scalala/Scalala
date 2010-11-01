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
import generic.{CanAdd,CanMul};

/**
 * Implementation trait for proxies to a Tensor1 instance.
 *
 * @author dramage
 */
trait Tensor1ProxyLike
[@specialized(Int,Long)A, @specialized(Int,Long,Float,Double) B,
 +D<:IterableDomain[A] with DomainLike[A,D], Inner<:Tensor1[A,B], +This<:Tensor1[A,B]]
extends TensorProxyLike[A,B,D,Inner,This] with Tensor1Like[A,B,D,This] {
  override def norm(n : Double) =
    inner.norm(n);

  override def dot[C,R](that : Tensor1[A,C])(implicit mul : CanMul[B,C,R], add : CanAdd[R,R,R], scalar : Scalar[R]) : R =
    inner.dot(that);
}

/**
 * Proxy to a Tensor1 instance.
 * 
 * @author dramage
 */
trait Tensor1Proxy
[@specialized(Int,Long)A, @specialized(Int,Long,Float,Double) B, Inner<:Tensor1[A,B]]
extends TensorProxy[A,B,Inner] with Tensor1[A,B] with Tensor1ProxyLike[A,B,IterableDomain[A],Inner,Tensor1Proxy[A,B,Inner]];
