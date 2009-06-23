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

/**
 * Shape parameters for Tensor1Ops.
 */
object Tensor1Op {
  final abstract class Col;
  final abstract class Row;
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
trait Tensor1Ops {
  protected val colTensor1OpBuilderImpl =
    new ColTensor1OpBuilderImpl[Any,Tensor1[Any],Tensor2[Any,Any]]();

  implicit def iColTensor1OpBuilderImpl[I,Bound1<:Tensor1[I],Bound2<:Tensor2[I,I]] =
    colTensor1OpBuilderImpl.asInstanceOf[ColTensor1OpBuilderImpl[I,Tensor1[I],Bound2]];

  implicit def iColTensor1OpToRichColTensor1Op[I,V<:Tensor1[I]]
  (op : TensorOp[I,Tensor1[I],V,Tensor1Op.Col])
  (implicit builder : ColTensor1OpBuilderImpl[I,Tensor1[I],Tensor2[I,I]]) =
    new RichColTensor1Op[I,Tensor1[I],V,Tensor2[I,I]](op)(builder);

  implicit def iTensor1ToRichColTensor1Op[I](x : Tensor1[I])
  (implicit builder : ColTensor1OpBuilderImpl[I,Tensor1[I],Tensor2[I,I]]) =
    iColTensor1OpToRichColTensor1Op(builder.mkTensorIdentity(x))(builder);

  protected val rowTensor1OpBuilder =
    new RowTensor1OpBuilderImpl[Any,Tensor1,Tensor2]();

  implicit def iRowTensor1OpBuilder[I] =
    rowTensor1OpBuilder.asInstanceOf[RowTensor1OpBuilderImpl[I,Tensor1,Tensor2]];

  implicit def iRowTensor1OpToRichRowTensor1Op[I]
  (op : RowTensor1Op[I,Tensor1[I],Tensor1[I]])
  (implicit builder : RowTensor1OpBuilderImpl[I,Tensor1,Tensor2]) =
    new RichRowTensor1Op[I,Tensor1,Tensor2,Tensor1[I]](op);
  
//  implicit def iColTensor1OpToTensor[I,V<:Tensor1[I]]
//  (x : TensorOp[I,Tensor[I],V,Tensor1Op.Col]) =
//    x.value;
}

/** Singleton instance of Tensor1Ops trait. */
object Tensor1Ops extends Tensor1Ops;


/** Operators for column tensors. */
trait ColTensor1OpBuilder[I,Bound1<:Tensor1[I],Bound2<:Tensor2[I,I]]
extends TensorOpBuilder[I,Bound1,Tensor1Op.Col] {
  def mkTensor1ColToRow[Value<:Bound1](op : ColTensor1Op[I,Bound1,Value]) =
    Tensor1ColToRow(op);
  
  def mkTensor1OuterMultTensor1[V1<:Bound1,V2<:Bound1](
    a : TensorOp[I,Bound1,V1,Tensor1Op.Col],
    b : TensorOp[I,Bound1,V2,Tensor1Op.Row]) =
    Tensor1OuterMultTensor1[I,Bound1,V1,V2,Bound2](a,b);
}

/** Implementation of ColTensor1OpBuilde. */
class ColTensor1OpBuilderImpl[I,Bound1<:Tensor1[I],Bound2<:Tensor2[I,I]]
extends ColTensor1OpBuilder[I,Bound1,Bound2];

/** Operators for column tensors. */
class RichColTensor1Op[I,Bound<:Tensor1[I],Value<:Bound,OuterMult<:Tensor2[I,I]]
(base : ColTensor1Op[I,Bound,Value])
(implicit ops : ColTensor1OpBuilder[I,Bound,OuterMult])
extends RichTensorOp[I,Bound,Value,Tensor1Op.Col](base) {
  /** Transposes this tensor to a row tensor. */
  def t = ops.mkTensor1ColToRow(base);
  
  /**
   * Outer multiplication.  This method has no paired method
   * for accepting raw values because raw values cannot be
   * RowTensors without an explicit transpose.
   */
  def * [V<:Bound] (op : RowTensor1Op[I,Bound,V]) =
    ops.mkTensor1OuterMultTensor1(base,op);
}

/** Operators for row tensors. */
trait RowTensor1OpBuilder[I,Bound1[X]<:Tensor1[X],Bound2[X,Y]<:Tensor2[X,Y]]
extends TensorOpBuilder[I,Bound1[I],Tensor1Op.Row] {
  def mkTensor1RowToCol[V<:Bound1[I]]
  (op : RowTensor1Op[I,Bound1[I],V]) =
    Tensor1RowToCol(op);
  
  def mkTensor1DotTensor1[V1<:Bound1[I],V2<:Bound1[I]]
  (a : RowTensor1Op[I,Bound1[I],V1], b : ColTensor1Op[I,Bound1[I],V2]) =
    a.value dot b.value;
  
  def mkRowTensor1MultTensor2[J,VO<:Bound1[J],V1<:Bound1[I],V2<:Bound2[I,J]]
  (a : RowTensor1Op[I,Bound1[I],V1], b : Tensor2Op[I,J,Bound2[I,J],V2]) =
    RowTensor1MultTensor2[I,J,Bound1[J],VO,Bound1[I],V1,Bound2[I,J],V2](a, b);
}

/** Operators for row tensors. */
class RowTensor1OpBuilderImpl[I,Bound1[X]<:Tensor1[X],Bound2[X,Y]<:Tensor2[X,Y]]
extends RowTensor1OpBuilder[I,Bound1,Bound2];

/** Operators for row tensors. */
class RichRowTensor1Op[I,BoundV[X]<:Tensor1[X],BoundM[X,Y]<:Tensor2[X,Y],Value<:BoundV[I]]
(base : RowTensor1Op[I,BoundV[I],Value])
(implicit ops : RowTensor1OpBuilder[I,BoundV,BoundM], colOps : ColTensor1OpBuilder[I,BoundV[I],BoundM[I,I]])
extends RichTensorOp[I,BoundV[I],Value,Tensor1Op.Row](base) {
  
  def t = ops.mkTensor1RowToCol(base);
  
  /** Inner multiplication. */
  def * [V<:BoundV[I]] (op : ColTensor1Op[I,BoundV[I],V]) =
    ops.mkTensor1DotTensor1(base, op);
  
  /** Inner multiplication. */
  def * [V<:BoundV[I]] (v : V) =
    ops.mkTensor1DotTensor1(base, colOps.mkTensorIdentity(v));
  
  /** Vector-matrix multiplication */
  def * [J,V<:BoundM[I,J]]
  (op : Tensor2Op[I,J,BoundM[I,J],V]) =
    ops.mkRowTensor1MultTensor2[J,BoundV[J],Value,V](base, op);
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
extends TensorOp[(I,I),Tensor2[I,I],OuterMult,(I,I)] {
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
