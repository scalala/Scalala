/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala.tensor.operators;

import scalala.collection.{MergeableSet, IntSpanSet, ProductSet, DomainException};
import scalala.tensor.{Tensor, Tensor1, Tensor2, Vector, Matrix};

import scalala.tensor.{Vector,Matrix};
import Tensor1Types._;

object VectorTypes {
  type ColVectorOp[V<:Vector] =
    ColTensor1Op[Int,Vector,V];

  type ColVectorOpBuilder[V<:Vector] =
    ColTensor1OpBuilder[Int,Vector,Matrix];

  type RichColVectorOp[V<:Vector] =
    RichColTensor1Op[Int,Vector,V,Matrix];

  type RowVectorOp[V<:Vector] =
    RowTensor1Op[Int,Vector,V];
}

import VectorTypes._;
import MatrixTypes._;


trait VectorOps {
  protected val colVectorOpBuilder =
    new ColVectorOpBuilder[Vector]();
  
  implicit def iColVectorOpBuilder[V<:Vector] =
    colVectorOpBuilder.asInstanceOf[ColVectorOpBuilder[V]];

  implicit def iColVectorOpTopRichColVectorOp
  (op : ColVectorOp[Vector])
  (implicit builder : ColVectorOpBuilder[Vector]) =
    new RichColVectorOp(op)(builder);

  implicit def iVectorToRichColVectorOp(x : Vector)
  (implicit builder : ColVectorOpBuilder[Vector]) =
    new RichColVectorOp[Vector](builder.mkTensorIdentity(x))(builder);

  protected val rowVectorOpBuilder =
    new RowVectorOpBuilder();
  
  implicit def iRowVectorOpBuilder[V<:Vector] =
    rowVectorOpBuilder.asInstanceOf[RowVectorOpBuilder[V]];

  implicit def iRowVectorOpToRichRowVectorOp[V<:Vector]
  (op : RowVectorOp[V])
  (implicit builder : RowVectorOpBuilder[V]) =
    new RichRowVectorOp[V](op)(builder);
  
//  implicit def iColVectorOpToVector[V<:Vector]
//  (op : ColVectorOp[V]) =
//    op.value;
}

object VectorOps extends VectorOps;


class RowVectorOpBuilder[V<:Vector]
extends TensorOpBuilder[Int,Vector,Tensor1Op.Row] {
  def mkVectorRowToCol[V<:Vector]
  (op : RowTensor1Op[Int,Vector,V]) =
    Tensor1RowToCol(op);
  
  def mkVectorDotVector[V1<:Vector,V2<:Vector]
  (a : RowVectorOp[V1], b : ColVectorOp[V2]) =
    a.value dot b.value;
  
  def mkRowVectorMultMatrix[VI<:Vector,M<:Matrix]
  (a : RowVectorOp[VI], b : MatrixOp[M]) =
    VectorMultMatrix[V,VI,M](a, b);
}

class RichRowVectorOp[V<:Vector]
(base : RowVectorOp[V])
(implicit ops : RowVectorOpBuilder[V])
extends RichTensorOp[Int,Vector,V,Tensor1Op.Row](base) {
  
  def t = ops.mkVectorRowToCol(base);
  
  /** Inner multiplication. */
  def * [V2<:Vector] (op : ColVectorOp[V2]) =
    ops.mkVectorDotVector(base, op);
  
  /** Inner multiplication. */
  def * [V2<:Vector]
  (v : V2)(implicit colOps : ColVectorOpBuilder[V2]) =
    ops.mkVectorDotVector(base, colOps.mkTensorIdentity(v));
  
  /** Vector-matrix multiplication */
  def * [M<:Matrix]
  (op : MatrixOp[M]) =
    ops.mkRowVectorMultMatrix[V,M](base, op);
  
  /** Vector-matrix multiplication */
  def * [M<:Matrix]
  (m : M)(implicit mOps : MatrixOpBuilder) =
    ops.mkRowVectorMultMatrix[V,M](base, mOps.mkTensorIdentity(m));
}

case class VectorMultMatrix[VO<:Vector,VI<:Vector,M<:Matrix]
(a : RowVectorOp[VI], b : MatrixOp[M])
extends RowTensor1Op[Int,Vector,VO] {
  override def domain = b.domain.asInstanceOf[ProductSet[Int,Int]]._2;
  override lazy val value = {
    val vv = a.value;
    val mv = b.value;
    val rv = vv.create(domain).asInstanceOf[VO];
    for (j <- domain) {
      rv(j) = vv dot mv.getCol(j);
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = b.create(d);
}
