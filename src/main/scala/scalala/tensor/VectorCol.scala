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
import generic.collection.{CanAppendColumns};

import scalala.operators._;

/**
 * Implementation trait for a row vector.
 *
 * @author dramage
 */
trait VectorColLike[@specialized(Int,Long,Float,Double) B, +This<:VectorCol[B]]
extends VectorLike[B,This] with Tensor1ColLike[Int,B,IndexDomain,This];

/**
 * A vector shaped as a row.
 *
 * @author dramage
 */
trait VectorCol[@specialized(Int,Long,Float,Double) B]
extends Vector[B] with Tensor1Col[Int,B] with VectorColLike[B,VectorCol[B]];

object VectorCol extends VectorColCompanion[VectorCol] {
  implicit def canTranspose[V] : UnaryOp[VectorCol[V],OpTranspose,VectorRow[V]]
  = new UnaryOp[VectorCol[V],OpTranspose,VectorRow[V]] {
    override def apply(col : VectorCol[V]) =
      new VectorRow.View[V](col);
  }

  class View[V](override val inner : Vector[V])
  extends VectorProxy[V,Vector[V]] with VectorCol[V]
  with VectorLike[V,View[V]] {
    override def repr : View[V] = this;
  }
}

trait VectorColCompanion[Bound[V]<:VectorCol[V]] extends VectorCompanion[Bound] {
  implicit def canMulVectorColByRow[V1,V2,RV]
  (implicit mul : BinaryOp[V1,V2,OpMul,RV], scalar : Scalar[RV])
  : BinaryOp[Bound[V1],VectorRow[V2],OpMulColVectorBy,Matrix[RV]]
  = new BinaryOp[Bound[V1],VectorRow[V2],OpMulColVectorBy,Matrix[RV]] {
    override def apply(a : Bound[V1], b : VectorRow[V2]) = {
      val builder = a.newBuilder(TableDomain(a.domain.size, b.domain.size));
      a.foreachNonZero((i,va) => b.foreachNonZero((j,vb) => builder((i,j)) = mul(va,vb)));
      builder.result.asInstanceOf[Matrix[RV]];
    }
  }

//  implicit def canAppendMatrixColumns[V]
//  : CanAppendColumns[Bound[V],Matrix[V],Matrix[V]]
//  = new CanAppendColumns[Bound[V],Matrix[V],Matrix[V]] {
//    override def apply(a : Bound[V], b : Matrix[V]) = {
//      require(a.size == b.numRows, "Arguments must have same number of rows");
//      implicit val sv = a.scalar;
//      val builder = a.newBuilder[(Int,Int),V](TableDomain(a.size, 1+b.numCols));
//      a.foreachNonZero((i,v) => builder((i,0)) = v);
//      b.foreachNonZero((i,j,v) => builder((i,j+1)) = v);
//      builder.result.asInstanceOf[Matrix[V]];
//    }
//  }

//  implicit def canAppendVectorColumn[V]
//  : CanAppendColumns[Bound[V],VectorCol[V],Matrix[V]]
//  = new CanAppendColumns[Bound[V],VectorCol[V],Matrix[V]] {
//    override def apply(a : Bound[V], b : VectorCol[V]) = {
//      require(a.size == b.size, "Arguments must have same number of rows");
//      implicit val sv = a.scalar;
//      val builder = a.newBuilder[(Int,Int),V](TableDomain(a.size, 2));
//      a.foreachNonZero((i,v) => builder((i,0)) = v);
//      b.foreachNonZero((i,v) => builder((i,1)) = v);
//      builder.result.asInstanceOf[Matrix[V]];
//    }
//  }
}

