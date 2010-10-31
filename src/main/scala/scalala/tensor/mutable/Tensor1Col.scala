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

import scalala.tensor.domain._;
import scalala.generic.collection.CanTranspose;

import scalar.Scalar;

/**
 * Implementation trait for mutable Tensor1Col instances.
 *
 * @author dramage
 */
trait Tensor1ColLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +D<:IterableDomain[K] with DomainLike[K,D], +This<:Tensor1Col[K,V]]
extends tensor.Tensor1ColLike[K,V,D,This] with Tensor1Like[K,V,D,This];

/**
 * Mutable tensor.Tensor1.
 *
 * @author dramage
 */
trait Tensor1Col
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends tensor.Tensor1Col[K,V] with Tensor1[K,V]
with Tensor1ColLike[K,V,IterableDomain[K],Tensor1Col[K,V]];

object Tensor1Col extends Tensor1ColCompanion[Tensor1Col] {
  /** Constructs an open-domain tensor seeded with the given values. */
  def apply[K,V:Scalar](values : (K,V)*) : Tensor1Col[K,V] = {
    new Impl[K,V](scala.collection.mutable.Map(values :_*)) {
      override def checkKey(key : K) = true;
    }
  }

  /** Constructs a closed-domain tensor for the given domain. */
  def apply[K,V:Scalar](domain : IterableDomain[K]) : Tensor1Col[K,V] = {
    val d = domain;
    new Impl[K,V](scala.collection.mutable.Map[K,V]()) {
      override val domain = d;
    }
  }

  class Impl[K,V:Scalar](map : scala.collection.Map[K,V])
  extends Tensor1.Impl[K,V](map) with Tensor1Col[K,V];

  class View[K,V](override val inner : Tensor1Row[K,V])
  extends Tensor1Proxy[K,V,Tensor1Row[K,V]] with Tensor1Col[K,V]
  with Tensor1Like[K,V,IterableDomain[K],View[K,V]] {
    override def repr : View[K,V] = this;
  }

  implicit def canTranspose[K,V] : CanTranspose[Tensor1Col[K,V],Tensor1Row[K,V]]
  = new CanTranspose[Tensor1Col[K,V],Tensor1Row[K,V]] {
    override def apply(col : Tensor1Col[K,V]) =
      new Tensor1Row.View[K,V](col);
  }
}

trait Tensor1ColCompanion[Bound[K,V]<:Tensor1Col[K,V]]
extends tensor.Tensor1ColCompanion[Bound] with Tensor1Companion[Bound];
