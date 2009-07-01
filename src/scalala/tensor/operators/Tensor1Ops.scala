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

import TensorShapes._;

/**
 * Shape parameters for Tensor1Ops.
 */
object Tensor1Op {
  type Col = TensorShapes.Shape1Col;
  type Row = TensorShapes.Shape1Row;
}

/**
 * Type aliases for Tensor1 support.
 */
object Tensor1Types {
  type ColTensor1Op[I,Bound<:Tensor[I],Value<:Bound] =
    TensorOp[I,Bound,Value,Tensor1Op.Col];

  type RowTensor1Op[I,Bound<:Tensor[I],Value<:Bound] =
    TensorOp[I,Bound,Value,Tensor1Op.Row];
}

import Tensor1Types._;
import Tensor2Types._;


/**
 * Implicits for row and column Tensor1's.
 */
trait Tensor1Ops extends TensorOps {
}

/** Singleton instance of Tensor1Ops trait. */
object Tensor1Ops extends Tensor1Ops;


/** Operators for column tensors. */
class RichColTensor1Op[I,Bound<:Tensor1[I],Value<:Bound,OuterMult<:Tensor2[I,I]]
(base : ColTensor1Op[I,Bound,Value])
extends RichTensorOp[I,Bound,Value,Tensor1Op.Col](base) {
  /** Transposes this tensor to a row tensor. */
  def t = Tensor1ColToRow(base);
  
  /**
   * Outer multiplication.  This method has no paired method
   * for accepting raw values because raw values cannot be
   * RowTensors without an explicit transpose.
   */
  def * [V<:Bound] (op : RowTensor1Op[I,Bound,V]) =
    Tensor1OuterMultTensor1[I,Bound,Value,V,OuterMult](base,op);
}

/** Operators for row tensors. */
class RichRowTensor1Op[I,Bound<:Tensor1[I],Value<:Bound]
(base : RowTensor1Op[I,Bound,Value])
extends RichTensorOp[I,Bound,Value,Tensor1Op.Row](base) {
  
  def t = Tensor1RowToCol(base);
  
  /** Inner multiplication. */
  def * [V<:Tensor1[I]] (op : ColTensor1Op[I,Tensor1[I],V]) =
    base.value dot op.value;
  
  /** Vector-matrix multiplication */
  // TODO: tighten this bound
  def * [J,M<:Tensor2[I,J]]
  (op : Tensor2Op[I,J,Tensor2[I,J],M]) =
    RowTensor1MultTensor2[I,J,Tensor1[J],Tensor1[J],Bound,Value,Tensor2[I,J],M](base, op);
}

/** Transposes a column to a row. */
case class Tensor1ColToRow[I,Bound<:Tensor1[I],Value<:Bound]
(op : TensorOp[I,Bound,Value,Tensor1Op.Col])
extends TensorOp[I,Bound,Value,Tensor1Op.Row] {
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : MergeableSet[J]) = op.create(d);
}

/** Transposes a row to a column. */
case class Tensor1RowToCol[I,Bound<:Tensor1[I],Value<:Bound]
(op : TensorOp[I,Bound,Value,Tensor1Op.Row])
extends TensorOp[I,Bound,Value,Tensor1Op.Col] {
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : MergeableSet[J]) = op.create(d);
}

/** Outer multiplication to create a Tensor2. */
case class Tensor1OuterMultTensor1[I,Bound<:Tensor1[I],V1<:Bound,V2<:Bound,OuterMult<:Tensor2[I,I]]
(a : TensorOp[I,Bound,V1,Tensor1Op.Col], b : TensorOp[I,Bound,V2,Tensor1Op.Row])
extends TensorOp[(I,I),Tensor2[I,I],OuterMult,Shape2[I,I]] {
  override def domain = ProductSet(a.domain,b.domain);
  override lazy val value = {
    val av = a.value;
    val bv = b.value;
    val rv = av.create(domain).asInstanceOf[OuterMult];
    for (i <- a.domain; j <- b.domain) {
      rv(i,j) = av(i) * bv(j);
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = a.create(d);
}

/** Row tensor to Tensor2 multiplication. */
case class RowTensor1MultTensor2[I,J,Bound<:Tensor1[J],Value<:Bound,Bound1<:Tensor1[I],Value1<:Bound1,Bound2<:Tensor2[I,J],Value2<:Bound2]
(a : RowTensor1Op[I,Bound1,Value1], b : Tensor2Op[I,J,Bound2,Value2])
extends RowTensor1Op[J,Bound,Value] {
  override def domain = b.domain.asInstanceOf[ProductSet[I,J]]._2;
  override lazy val value = {
    val vv = a.value;
    val mv = b.value;
    val rv = vv.create(domain).asInstanceOf[Value];
    for (j <- domain) {
      rv(j) = vv dot mv.getCol(j);
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = b.create(d);
}
