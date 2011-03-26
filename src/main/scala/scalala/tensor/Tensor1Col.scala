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
import mutable.TensorBuilder;

import scalala.operators._;
import scalala.scalar.Scalar;
import scalala.generic.collection.CanBuildTensorFrom;

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
  
  def t : Tensor1Row[K,V] =
    new Tensor1Row.View[K,V](repr);
}

/**
 * One-axis tensor shaped as a row.
 *
 * @author dramage
 */
trait Tensor1Col[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor1[K,V] with Tensor1ColLike[K,V,IterableDomain[K],Tensor1Col[K,V]];

object Tensor1Col {
  class View[K,V](override val inner : Tensor1Row[K,V])
  extends Tensor1Proxy[K,V,Tensor1Row[K,V]] with Tensor1Col[K,V]
  with Tensor1Like[K,V,IterableDomain[K],View[K,V]] {
    override def repr : View[K,V] = this;
  }
  
  implicit def canMulTensor1ColByRow[K1,K2,V1,V2,RV,A,DA,B,DB,DThat,That]
  (implicit viewA : A=>Tensor1Col[K1,V1], viewB : B=>Tensor1Row[K2,V2],
   dA : DomainFor[A,DA], dB : DomainFor[B,DB], dThat : CanGetProduct2DomainFor[DA,DB,DThat],
   mul : BinaryOp[V1,V2,OpMul,RV],
   bf : CanBuildTensorFrom[A, DThat, (K1,K2), RV, That])
  : BinaryOp[A,B,OpMulColVectorBy,That]
  = new BinaryOp[A,B,OpMulColVectorBy,That] {
    override def apply(a : A, b : B) = {
      val builder = bf(a, dThat(a.domain.asInstanceOf[DA], b.domain.asInstanceOf[DB]));
      a.foreachNonZero((i,va) => b.foreachNonZero((j,vb) => builder((i,j)) = mul(va,vb)));
      builder.result;
    }
  }
}

