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
import generic.{CanAdd,CanMul,CanMulRowBy};
import generic.collection.{CanTranspose,CanSliceCol};
import mutable.TensorBuilder;

/**
 * Implementation trait for a one-axis tensor shaped as a row.
 *
 * @author dramage
 */
trait Tensor1RowLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +D<:IterableDomain[K] with DomainLike[K,D], +This<:Tensor1Row[K,V]]
extends Tensor1Like[K,V,D,This] with operators.RowOps[This] { self =>
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = domain match {
    case that : IndexDomain =>
      mutable.Vector[V2](that).t.asBuilder;
    case that : Product1Domain[_] =>
      mutable.Tensor1Row[K2,V2](that).asBuilder;
    case _ =>
      super.newBuilder[K2,V2](domain);
  }
}

/**
 * One-axis tensor shaped as a row.
 *
 * @author dramage
 */
trait Tensor1Row[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor1[K,V] with Tensor1RowLike[K,V,IterableDomain[K],Tensor1Row[K,V]];

object Tensor1Row extends Tensor1RowCompanion[Tensor1Row] {
  implicit def canTranspose[K,V] : CanTranspose[Tensor1Row[K,V],Tensor1Col[K,V]]
  = new CanTranspose[Tensor1Row[K,V],Tensor1Col[K,V]] {
    override def apply(row : Tensor1Row[K,V]) =
      new Tensor1Col.View[K,V](row);
  }

  class View[K,V](override val inner : Tensor1Col[K,V])
  extends Tensor1Proxy[K,V,Tensor1Col[K,V]] with Tensor1Row[K,V]
  with Tensor1Like[K,V,IterableDomain[K],View[K,V]] {
    override def repr : View[K,V] = this;
  }
}

trait Tensor1RowCompanion[Bound[K,V]<:Tensor1Row[K,V]] extends Tensor1Companion[Bound] {
  implicit def canMulTensor1RowByCol[K,V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], scalar : Scalar[RV])
  : CanMulRowBy[Bound[K,V1],Tensor1Col[K,V2],RV]
  = new CanMulRowBy[Bound[K,V1],Tensor1[K,V2],RV] {
    override def apply(a : Bound[K,V1], b : Tensor1[K,V2]) =
      a dot b;
  }

  implicit def canMulTensor1RowByMatrix[K1,K2,V1,V2,Col,RV]
  (implicit slice : CanSliceCol[scalala.tensor.Tensor2[K1,K2,V2],K2,Col],
   mul : CanMulRowBy[Bound[K1,V1],Col,RV], scalar : Scalar[RV])
  : CanMulRowBy[Bound[K1,V1],Tensor2[K1,K2,V2],Tensor1Row[K2,RV]]
  = new CanMulRowBy[Bound[K1,V1],Tensor2[K1,K2,V2],Tensor1Row[K2,RV]] {
    override def apply(a : Bound[K1,V1], b : Tensor2[K1,K2,V2]) = {
      val builder = a.newBuilder[K2,RV](b.domain._2);
      for (j <- b.domain._2) {
        builder(j) = mul(a, b(::, j));
      }
      builder.result.asInstanceOf[Tensor1Row[K2,RV]];
    }
  }
}
