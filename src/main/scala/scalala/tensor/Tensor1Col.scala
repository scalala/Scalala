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
import generic.{CanAdd,CanMul,CanMulColumnBy};
import generic.collection.CanTranspose;
import mutable.TensorBuilder;

/**
 * Implementation trait for a one-axis tensor shaped as a row.
 *
 * @author dramage
 */
trait Tensor1ColLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +D<:IterableDomain[K] with DomainLike[K,D], +This<:Tensor1Col[K,V]]
extends Tensor1Like[K,V,D,This] with operators.ColOps[This] { self =>
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = domain match {
    case that : IndexDomain =>
      mutable.Vector[V2](that).asBuilder;
    case that : Product1Domain[_] =>
      mutable.Tensor1Col[K2,V2](that).asBuilder;
    case _ =>
      super.newBuilder[K2,V2](domain);
  }
}

/**
 * One-axis tensor shaped as a row.
 *
 * @author dramage
 */
trait Tensor1Col[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor1[K,V] with Tensor1ColLike[K,V,IterableDomain[K],Tensor1Col[K,V]];

object Tensor1Col extends Tensor1ColCompanion[Tensor1Col] {
  implicit def canTranspose[K,V] : CanTranspose[Tensor1Col[K,V],Tensor1Row[K,V]]
  = new CanTranspose[Tensor1Col[K,V],Tensor1Row[K,V]] {
    override def apply(col : Tensor1Col[K,V]) =
      new Tensor1Row.View[K,V](col);
  }

  class View[K,V](override val inner : Tensor1Row[K,V])
  extends Tensor1Proxy[K,V,Tensor1Row[K,V]] with Tensor1Col[K,V]
  with Tensor1Like[K,V,IterableDomain[K],View[K,V]] {
    override def repr : View[K,V] = this;
  }
}

trait Tensor1ColCompanion[Bound[K,V]<:Tensor1Col[K,V]] extends Tensor1Companion[Bound];
