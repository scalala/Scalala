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
import generic.collection.{CanSliceCol};
import mutable.TensorBuilder;

import scalala.operators._;

/**
 * Implementation trait for a row vector.
 *
 * @author dramage
 */
trait VectorRowLike[@specialized(Int,Long,Float,Double) B, +This<:VectorRow[B]]
extends VectorLike[B,This] with Tensor1RowLike[Int,B,IndexDomain,This] {

  // TODO: improve this method to make it more Vector-like
  override def toString = {
    val rv = valuesIterator.take(10).map(mkValueString).mkString(" ");
    if (size > 10) {
      rv + " " + "... ("+(size-10) +" more)";
    } else {
      rv;
    }
  }
}

/**
 * A vector shaped as a row.
 *
 * @author dramage
 */
trait VectorRow[@specialized(Int,Long,Float,Double) B]
extends Vector[B] with Tensor1Row[Int,B] with VectorRowLike[B,VectorRow[B]];

object VectorRow extends VectorRowCompanion[VectorRow] {
  implicit def canTranspose[V] : UnaryOp[VectorRow[V],OpTranspose,VectorCol[V]]
  = new UnaryOp[VectorRow[V],OpTranspose,VectorCol[V]] {
    override def apply(row : VectorRow[V]) =
      new VectorCol.View[V](row);
  }

  class View[V](override val inner : Vector[V])
  extends VectorProxy[V,Vector[V]] with VectorRow[V]
  with VectorLike[V,View[V]] {
    override def repr : View[V] = this;
  }
}

trait VectorRowCompanion[Bound[V]<:VectorRow[V]] extends VectorCompanion[Bound] {
//  implicit def canMulVectorRowByCol[V1,V2,RV]
//  (implicit mul : BinaryOp[V1,V2,OpMul,RV], add : BinaryOp[RV,RV,OpAdd,RV], scalar : Scalar[RV])
//  : BinaryOp[Bound[V1],Tensor1Col[Int,V2],OpMulRowVectorBy,RV]
//  = new BinaryOp[Bound[V1],Tensor1Col[Int,V2],OpMulRowVectorBy,RV] {
//    override def apply(a : Bound[V1], b : Tensor1Col[Int,V2]) =
//      a dot b;
//  }

  implicit def canMulVectorRowByMatrix[V1,V2,Col,RV]
  (implicit slice : CanSliceCol[Matrix[V2],Int,Col],
   mul : BinaryOp[Bound[V1],Col,OpMulRowVectorBy,RV], scalar : Scalar[RV])
  : BinaryOp[Bound[V1],Matrix[V2],OpMulRowVectorBy,VectorRow[RV]]
  = new BinaryOp[Bound[V1],Matrix[V2],OpMulRowVectorBy,VectorRow[RV]] {
    override def apply(a : Bound[V1], b : Matrix[V2]) = {
      val rv = a.newBuilder[Int,RV](domain.IndexDomain(b.numCols));
      var j = 0;
      while (j < b.numCols) {
        rv(j) = mul(a, b(::, j));
        j += 1;
      }
      rv.result.asInstanceOf[VectorRow[RV]];
    }
  }
}

