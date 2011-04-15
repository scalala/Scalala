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
package mutable;

import domain.{IndexDomain,TableDomain};

import scalala.scalar.Scalar;
import scalala.generic.collection._;
import scalala.operators._;

/**
 * Implementation trait for a MutableTensor that is also a tensor.Matrix.
 *
 * @author dramage
 */
trait MatrixLike
[@specialized(Int,Long,Float,Double,Boolean) V, +This<:Matrix[V]]
extends tensor.MatrixLike[V,This]
with Tensor2Like[Int,Int,V,IndexDomain,IndexDomain,TableDomain,TableDomain,This] {
  override def t : Matrix[V] =
    new MatrixTranspose.Impl[V,This](repr);
}

/**
 * MutableTensor that is also a tensor.Matrix.
 *
 * @author dramage
 */
trait Matrix
[@specialized(Int,Long,Float,Double,Boolean) B]
extends tensor.Matrix[B]
with Tensor2[Int,Int,B]
with MatrixLike[B,Matrix[B]];

object Matrix extends dense.DenseMatrixConstructors {
  implicit def canSliceRow[V:Scalar] : CanSliceRow[Matrix[V],Int,VectorRow[V]]
  = new CanSliceRow[Matrix[V],Int,VectorRow[V]] {
    override def apply(from : Matrix[V], row : Int) =
      new RowSliceImpl[V,Matrix[V]](from,row);
  }

  implicit def canSliceCol[V:Scalar] : CanSliceCol[Matrix[V],Int,VectorCol[V]]
  = new CanSliceCol[Matrix[V],Int,VectorCol[V]] {
    override def apply(from : Matrix[V], col : Int) =
      new ColSliceImpl[V,Matrix[V]](from, col);
  }

  implicit def canSliceMatrix[V:Scalar] : CanSliceMatrix[Matrix[V],Int,Int,Matrix[V]]
  = new CanSliceMatrix[Matrix[V],Int,Int,Matrix[V]] {
    override def apply(from : Matrix[V], keys1 : Seq[Int], keys2 : Seq[Int]) =
      new MatrixSliceImpl[V,Matrix[V]](from, keys1, keys2);
  }

  trait RowSliceLike[V,+Coll<:Matrix[V],+This<:RowSlice[V,Coll]]
  extends tensor.Matrix.RowSliceLike[V,Coll,This]
  with VectorSliceLike[(Int,Int),TableDomain,V,Coll,This] with VectorRowLike[V,This];

  trait RowSlice[V,+Coll<:Matrix[V]]
  extends tensor.Matrix.RowSlice[V,Coll] with VectorSlice[(Int,Int),V,Coll]
  with VectorRow[V] with RowSliceLike[V,Coll,RowSlice[V,Coll]];

  class RowSliceImpl[V,+Coll<:Matrix[V]]
  (override val underlying : Coll, override val row : Int)
  (implicit override val scalar : Scalar[V])
  extends tensor.Matrix.RowSliceImpl[V,Coll](underlying,row)
  with RowSlice[V,Coll];

  trait ColSliceLike[V,+Coll<:Matrix[V],+This<:ColSlice[V,Coll]]
  extends tensor.Matrix.ColSliceLike[V,Coll,This]
  with VectorSliceLike[(Int,Int),TableDomain,V,Coll,This] with VectorColLike[V,This];

  trait ColSlice[V,+Coll<:Matrix[V]]
  extends tensor.Matrix.ColSlice[V,Coll] with VectorSlice[(Int,Int),V,Coll]
  with VectorCol[V] with ColSliceLike[V,Coll,ColSlice[V,Coll]];

  class ColSliceImpl[V,+Coll<:Matrix[V]]
  (override val underlying : Coll, override val col : Int)
  (implicit override val scalar : Scalar[V])
  extends tensor.Matrix.ColSliceImpl[V,Coll](underlying,col)
  with ColSlice[V,Coll];

  trait MatrixSliceLike[@specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Matrix[V], +This<:MatrixSlice[V,Coll]]
  extends tensor.Matrix.MatrixSliceLike[V,Coll,This]
  with TensorSliceLike[(Int,Int),TableDomain,(Int,Int),TableDomain,V,Coll,This]
  with MatrixLike[V,This] {
    override def update(i : Int, j : Int, value : V) =
      underlying.update(lookup1(i), lookup2(j), value);
  }

  trait MatrixSlice[@specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Matrix[V]]
  extends tensor.Matrix.MatrixSlice[V,Coll] with TensorSlice[(Int,Int),(Int,Int),V,Coll]
  with Matrix[V] with MatrixSliceLike[V,Coll,MatrixSlice[V,Coll]];

  class MatrixSliceImpl[V, +Coll<:Matrix[V]]
  (override val underlying : Coll, override val keys1 : Seq[Int], override val keys2 : Seq[Int])
  (implicit override val scalar : Scalar[V])
  extends tensor.Matrix.MatrixSliceImpl[V,Coll](underlying,keys1,keys2)
  with MatrixSlice[V, Coll];
}

