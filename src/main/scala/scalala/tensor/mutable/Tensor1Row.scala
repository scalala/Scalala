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

import domain.{Domain1,IndexDomain};

import scalala.generic.collection.{CanSliceVector};

import scalala.scalar.Scalar;

/**
 * Implementation trait for mutable Tensor1Row instances.
 *
 * @author dramage
 */
trait Tensor1RowLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +D<:Domain1[K], +This<:Tensor1Row[K,V]]
extends tensor.Tensor1RowLike[K,V,D,This] with Tensor1Like[K,V,D,This] {

  override def t : Tensor1Col[K,V] =
    new Tensor1Col.View[K,V](repr);
}

/**
 * Mutable tensor.Tensor1Row.
 *
 * @author dramage
 */
trait Tensor1Row
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends tensor.Tensor1Row[K,V] with Tensor1[K,V]
with Tensor1RowLike[K,V,Domain1[K],Tensor1Row[K,V]];

object Tensor1Row {
  /** Constructs a closed-domain tensor for the given domain. */
  def apply[K,V:Scalar](domain : Domain1[K]) : Tensor1Row[K,V] = domain match {
    case d : IndexDomain => VectorRow(d);
    case _ => new Impl(domain, scala.collection.mutable.Map[K,V]());
  }

  class Impl[K,V:Scalar](
    override val domain : Domain1[K],
    override protected val data : scala.collection.mutable.Map[K,V])
  extends Tensor.Impl[K,V](domain, data) with Tensor1Row[K,V];
  
  class View[K,V](override val inner : Tensor1Col[K,V])
  extends Tensor1Proxy[K,V,Tensor1Col[K,V]] with Tensor1Row[K,V]
  with Tensor1Like[K,V,Domain1[K],View[K,V]] {
    override def repr : View[K,V] = this;
    override def t : Tensor1Col[K,V] = inner;
  }
  
  implicit def canSliceVectorRow[K, V:Scalar] =
  new CanSliceVector[Tensor[K,V], K, VectorRow[V]] {
    override def apply(from : Tensor[K,V], keys : Seq[K]) =
      new VectorRowSlice.FromKeySeq[K,V,Tensor[K,V]](from, keys);
  }
}

