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
  
  type RowVectorOp[V<:Vector] =
    RowTensor1Op[Int,Vector,V];
}

import VectorTypes._;
import MatrixTypes._;

trait VectorOps extends TensorOps {
}

object VectorOps extends VectorOps;


class RichColVectorOp[V<:Vector](base : ColVectorOp[V])
extends RichColTensor1Op[Int,Vector,V,Matrix](base);

class RichRowVectorOp[V<:Vector](base : RowVectorOp[V])
extends RichTensorOp[Int,Vector,V,Tensor1Op.Row](base) {
  
  /** Transpose to column vector. */
  def t = Tensor1RowToCol(base);
  
  /** Inner multiplication. */
  def * [V2<:Vector] (op : ColVectorOp[V2]) =
    base.value dot op.value;
  
  /** Vector-matrix multiplication */
  def * [M<:Matrix] (op : MatrixOp[M]) =
    RowVectorMultMatrix[V,V,M](base, op);
}

case class RowVectorMultMatrix[VO<:Vector,VI<:Vector,M<:Matrix]
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
