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

import domain.{IterableDomain,Domain1,Domain2,IndexDomain};
import generic.TensorBuilder;

import scalala.generic.collection.{CanSliceCol,CanBuildTensorFrom};
import scalala.scalar.Scalar;
import scalala.operators._;

/**
 * Implementation trait for a one-axis tensor shaped as a row.
 *
 * @author dramage
 */
trait Tensor1RowLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +D<:Domain1[K], +This<:Tensor1Row[K,V]]
extends Tensor1Like[K,V,D,This] with operators.RowOps[This] { self =>
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = domain match {
    case that : IndexDomain =>
      mutable.VectorRow[V2](that).asBuilder;
    case that : Domain1[_] =>
      mutable.Tensor1Row[K2,V2](that).asBuilder;
    case _ =>
      super.newBuilder[K2,V2](domain);
  }
  
  def t : Tensor1Col[K,V] =
    new Tensor1Col.View[K,V](repr);
}

/**
 * One-axis tensor shaped as a row.
 *
 * @author dramage
 */
trait Tensor1Row[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V]
extends Tensor1[K,V] with Tensor1RowLike[K,V,Domain1[K],Tensor1Row[K,V]];

object Tensor1Row {
  class View[K,V](override val inner : Tensor1Col[K,V])
  extends Tensor1Proxy[K,V,Tensor1Col[K,V]] with Tensor1Row[K,V]
  with Tensor1Like[K,V,Domain1[K],View[K,V]] {
    override def repr : View[K,V] = this;
    override def t : Tensor1Col[K,V] = inner;
  }
  
  implicit def canMulTensor1RowByCol[K,V1,V2,A,B,RV]
  (implicit viewA : A => Tensor1Row[K,V1], viewB : B => Tensor1Col[K,V2],
   mul : BinaryOp[V1,V2,OpMul,RV], add : BinaryOp[RV,RV,OpAdd,RV], scalar : Scalar[RV])
  : BinaryOp[A,B,OpMulRowVectorBy,RV]
  = new BinaryOp[A,B,OpMulRowVectorBy,RV] {
    override def opType = OpMulRowVectorBy;
    override def apply(a : A, b : B) =
      a dot b;
  }

  implicit def canMulTensor1RowByMatrix[K1,K2,V1,V2,Col,RV,ThisA,ThisB,D2<:Domain1[K2],That]
  (implicit viewA : ThisA => Tensor1Row[K1,V1],
   viewB : ThisB => Tensor2Like[K1,K2,V2,_,D2,_,_,_],
   slice : CanSliceCol[ThisB,K2,Col],
   mul : BinaryOp[ThisA,Col,OpMulRowVectorBy,RV], scalar : Scalar[RV],
   bf : CanBuildTensorFrom[ThisA,D2,K2,RV,That])
  : BinaryOp[ThisA,ThisB,OpMulRowVectorBy,That]
  = new BinaryOp[ThisA,ThisB,OpMulRowVectorBy,That] {
    override def opType = OpMulRowVectorBy;
    override def apply(a : ThisA, b : ThisB) = {
      val domain = b.domain.asInstanceOf[Domain2[_,_]]._2.asInstanceOf[D2];
      val builder : TensorBuilder[K2,RV,That] = bf(a, domain);
      for (j <- domain.asInstanceOf[Domain1[K2]]) {
        builder(j) = mul(a, slice(b, j));
      }
      builder.result;
    }
  }
}

