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
package scalala;
package tensor;
package operators;

import collection.{MergeableSet, IntSpanSet, ProductSet, DomainException};
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
  type ColTensor1Op[+Value<:Tensor1[_]] =
    TensorOp[Value,Tensor1Op.Col];

  type RowTensor1Op[+Value<:Tensor1[_]] =
    TensorOp[Value,Tensor1Op.Row];
}

import Tensor1Types._;
import Tensor2Types._;


/**
 * Implicits for row and column Tensor1's.
 */
trait Tensor1Ops extends TensorOps {
  implicit def tensor1ColArith[I] = new Tensor1Arith[I,Tensor1[I],Tensor1[I],Shape1Col];
  implicit def tensor1RowArith[I] = new TensorArith[I,Tensor1[I],Tensor1[I],Shape1Row];

}

/** Singleton instance of Tensor1Ops trait. */
object Tensor1Ops extends Tensor1Ops;


/** Operators for column tensors. */
class RichColTensor1Op[Value<:Tensor1[_]]
(base : ColTensor1Op[Value])
extends RichTensorOp[Value,Shape1Col](base) {
  /** Transposes this tensor to a row tensor. */
  def t = Tensor1ColToRow(base);
  
  /**
   * Outer multiplication.  This method has no paired method
   * for accepting raw values because raw values cannot be
   * RowTensors without an explicit transpose.
   */
  def * [V<:Tensor1[_],OuterMult<:Tensor2[_,_]] (op : RowTensor1Op[V])
  (implicit ops: TensorProductBuilder[Value,V,OuterMult,Shape1Col,Shape1Row,Shape2]) =
    ops.makeProduct(base,op);
}

/** Operators for row tensors. */
class RichRowTensor1Op[Value<:Tensor1[_]]
(base : RowTensor1Op[Value])
extends RichTensorOp[Value,Tensor1Op.Row](base) {
  
  def t = Tensor1RowToCol(base);
  
  /** Inner multiplication. */
  def * [V<:Tensor1[_]] (op : ColTensor1Op[V])(implicit ops: Tensor1Arith[_,Value,V,Shape1Row]) =
    ops.dotProduct(base.value, op.value);
  
  /** Vector-matrix multiplication */
  def * [M<:Tensor2[_,_],R<:Tensor1[_]] 
  (op : Tensor2Op[M])
  (implicit ops: TensorProductBuilder[Value,M,R,Shape1Row,Shape2,Shape1Row]) =
    ops.makeProduct(base, op);
}

/** Transposes a column to a row. */
case class Tensor1ColToRow[Value<:Tensor1[_]]
(op : TensorOp[Value,Tensor1Op.Col])
extends TensorOp[Value,Tensor1Op.Row] {
  override def value = op.value;
  override def working = op.working;
}

/** Transposes a row to a column. */
case class Tensor1RowToCol[Value<:Tensor1[_]]
(op : TensorOp[Value,Tensor1Op.Row])
extends TensorOp[Value,Tensor1Op.Col] {
  override def value = op.value;
  override def working = op.working;
}

/** Outer multiplication to create a Tensor2. */
case class Tensor1OuterMultTensor1[I,J,V1<:Tensor1[I],V2<:Tensor1[J],OuterMult<:Tensor2[I,J]]
(a : TensorOp[V1,Tensor1Op.Col], b : TensorOp[V2,Tensor1Op.Row])
(implicit tpB: TensorProductBuilder[V1,V2,OuterMult,Shape1Col,Shape1Row,Shape2])
extends TensorOp[OuterMult,Shape2] {
  override lazy val value = {
    val av = a.value;
    val bv = b.value;
    val rv = tpB.create(av,bv);
    for (i <- av.domain; j <- bv.domain) {
      rv(i,j) = av(i) * bv(j);
    }
    rv;
  }
}

/** Row tensor to Tensor2 multiplication. */
case class RowTensor1MultTensor2[I,J,Value<:Tensor1[J],Value1<:Tensor1[I],Value2<:Tensor2[I,J]]
(a : RowTensor1Op[Value1], b : Tensor2Op[Value2])
(implicit ops: TensorProductBuilder[Value1,Value2,Value,Shape1Row,Shape2,Shape1Row])
extends RowTensor1Op[Value] {
  override lazy val value = {
    val vv = a.value;
    val mv = b.value;
    val rv = ops.create(vv,mv);
    for (j <- mv.domain._2) {
      rv(j) = vv dot mv.getCol(j);
    }
    rv;
  }
}

class Tensor1Arith[I,V<:Tensor1[I],-V2<:Tensor1[I],S<:TensorShape] extends TensorArith[I,V,V2,S] {
  def dotProduct(v: V, v2: V2):Double = v dot v2;
}
