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
import generic.collection.{CanTranspose,CanSliceRow,CanSliceCol};
import scalar.Scalar;

/**
 * Implementation trait for a MutableTensor that is also a tensor.Matrix.
 *
 * @author dramage
 */
trait MatrixLike
[@specialized(Int,Long,Float,Double,Boolean) B, +This<:Matrix[B]]
extends tensor.MatrixLike[B,This]
with Tensor2Like[Int,Int,B,IndexDomain,IndexDomain,TableDomain,TableDomain,This];

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

object Matrix extends MatrixCompanion[Matrix] with dense.DenseMatrixConstructors {
  implicit def canTranspose[B:Scalar] : CanTranspose[Matrix[B], MatrixTranspose[B,Matrix[B]]] =
  new CanTranspose[Matrix[B], MatrixTranspose[B,Matrix[B]]] {
    override def apply(from : Matrix[B]) = new MatrixTranspose.Impl[B,Matrix[B]](from);
  }

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

  trait RowSliceLike[V,+Coll<:Matrix[V],+This<:RowSlice[V,Coll]]
  extends VectorSliceLike[(Int,Int),TableDomain,V,Coll,This] with VectorRowLike[V,This] {
    def row : Int;
    override val domain = underlying.domain._2;
    override def lookup(key : Int) = (row,key);
  }

  trait RowSlice[V,+Coll<:Matrix[V]]
  extends VectorSlice[(Int,Int),V,Coll] with VectorRow[V] with RowSliceLike[V,Coll,RowSlice[V,Coll]];

  class RowSliceImpl[V,+Coll<:Matrix[V]]
  (override val underlying : Coll, override val row : Int)
  (implicit override val scalar : Scalar[V])
  extends RowSlice[V,Coll];

  trait ColSliceLike[V,+Coll<:Matrix[V],+This<:ColSlice[V,Coll]]
  extends VectorSliceLike[(Int,Int),TableDomain,V,Coll,This] with VectorColLike[V,This] {
    def col : Int;
    override val domain = underlying.domain._2;
    override def lookup(key : Int) = (col,key);
  }

  trait ColSlice[V,+Coll<:Matrix[V]]
  extends VectorSlice[(Int,Int),V,Coll] with VectorCol[V] with ColSliceLike[V,Coll,ColSlice[V,Coll]];

  class ColSliceImpl[V,+Coll<:Matrix[V]]
  (override val underlying : Coll, override val col : Int)
  (implicit override val scalar : Scalar[V])
  extends ColSlice[V,Coll];
}

trait MatrixCompanion[Bound[V]<:Matrix[V]]
extends tensor.MatrixCompanion[Bound] with IndexedTensorCompanion[(Int,Int),Bound];
