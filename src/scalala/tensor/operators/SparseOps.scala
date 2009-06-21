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

import scalala.collection.{MergeableSet,ProductSet};
import scalala.tensor.{Vector,Matrix};
import scalala.tensor.sparse._;

import VectorTypes._;
import MatrixTypes._;

/** Type aliases supporting DenseVector operators. */
object SparseBinaryVectorTypes {
  type ColSparseBinaryVectorOp[V<:SparseBinaryVector] =
    ColVectorOp[V];
  
  type RowSparseBinaryVectorOp[V<:SparseBinaryVector] =
    RowVectorOp[V];
 }

import SparseBinaryVectorTypes._;

/** Implicits supporting DenseVector operations. */
trait SparseBinaryVectorOps {
  implicit val colSparseBinaryVectorOpBuilder =
    new ColSparseBinaryVectorOpBuilder();

  implicit def iColSparseBinaryVectorOpToRichColVectorOp[V<:SparseBinaryVector]
  (op : ColSparseBinaryVectorOp[V])
  (implicit builder : ColSparseBinaryVectorOpBuilder) =
    new RichColVectorOp(op)(builder);

  implicit def iSparseBinaryVectorToRichColVectorOp(x : SparseBinaryVector)
  (implicit builder : ColSparseBinaryVectorOpBuilder) =
    new RichColVectorOp(builder.mkTensorIdentity(x))(builder);

  protected val rowSparseBinaryVectorOpBuilder =
    new RowSparseBinaryVectorOpBuilder();
  
  implicit def iRowSparseBinaryVectorOpBuilder =
    rowSparseBinaryVectorOpBuilder.asInstanceOf[RowSparseBinaryVectorOpBuilder];

  implicit def iRowSparseBinaryVectorOpToRichRowDenseVectorOp
  (op : RowSparseBinaryVectorOp[SparseBinaryVector])
  (implicit builder : RowSparseBinaryVectorOpBuilder) =
    new RichRowVectorOp(op)(builder);
}

/** Singleton instance of DenseVectorOps trait. */
object SparseBinaryVectorOps extends SparseBinaryVectorOps;

class ColSparseBinaryVectorOpBuilder
extends ColVectorOpBuilder[SparseBinaryVector] {
  override def mkTensorIdentity[VV<:Vector](tensor : VV)
  : TensorIdentity[Int,Vector,VV,Tensor1Op.Col] =
    SparseColBinaryTensorIdentity(tensor.asInstanceOf[SparseBinaryVector]).asInstanceOf[TensorIdentity[Int,Vector,VV,Tensor1Op.Col]];
}

class RowSparseBinaryVectorOpBuilder
extends RowVectorOpBuilder[SparseBinaryVector];

case class SparseColBinaryTensorIdentity[Value<:SparseBinaryVector]
(override val tensor : Value)
extends TensorIdentity[Int,Vector,Value,Tensor1Op.Col](tensor) {
  override def create[J](d : MergeableSet[J]) =
    SparseVector.create(d);
}
