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
import generic.collection.{CanTranspose,CanSliceRow,CanSliceCol,CanSliceMatrix,CanAppendColumns};
import scalar.Scalar;

import operators._;

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
  implicit def canTranspose[B:Scalar] : UnaryOp[Matrix[B],OpTranspose,Matrix[B]] =
  new UnaryOp[Matrix[B],OpTranspose,Matrix[B]] {
    override def apply(from : Matrix[B]) = {
      if (from.isInstanceOf[MatrixTranspose[_,_]]) {
        from.asInstanceOf[MatrixTranspose[_,_]].underlying.asInstanceOf[Matrix[B]]
      } else {
        new MatrixTranspose.Impl[B,Matrix[B]](from);
      }
    }
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

  implicit def canSliceMatrix[V:Scalar] : CanSliceMatrix[Matrix[V],Int,Int,Matrix[V]]
  = new CanSliceMatrix[Matrix[V],Int,Int,Matrix[V]] {
    override def apply(from : Matrix[V], keys1 : Seq[Int], keys2 : Seq[Int]) =
      new MatrixSliceImpl[V,Matrix[V]](from, keys1, keys2);
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
    override val domain = underlying.domain._1;
    override def lookup(key : Int) = (key,col);
  }

  trait ColSlice[V,+Coll<:Matrix[V]]
  extends VectorSlice[(Int,Int),V,Coll] with VectorCol[V] with ColSliceLike[V,Coll,ColSlice[V,Coll]];

  class ColSliceImpl[V,+Coll<:Matrix[V]]
  (override val underlying : Coll, override val col : Int)
  (implicit override val scalar : Scalar[V])
  extends ColSlice[V,Coll];

  trait MatrixSliceLike[@specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Matrix[V], +This<:MatrixSlice[V,Coll]]
  extends TensorSliceLike[(Int,Int),TableDomain,(Int,Int),TableDomain,V,Coll,This]
  with MatrixLike[V,This] {

    def lookup1(i : Int) : Int;
    def lookup2(j : Int) : Int;

    /* final */ override def lookup(tup : (Int,Int)) =
      (lookup1(tup._1), lookup2(tup._2));

    override def apply(i : Int, j : Int) : V =
      underlying.apply(lookup1(i), lookup2(j));

    override def update(i : Int, j : Int, value : V) =
      underlying.update(lookup1(i), lookup2(j), value);
  }

  trait MatrixSlice[@specialized(Int,Long,Float,Double,Boolean) V,
   +Coll<:Matrix[V]]
  extends TensorSlice[(Int,Int),(Int,Int),V,Coll]
  with Matrix[V] with MatrixSliceLike[V,Coll,MatrixSlice[V,Coll]];

  class MatrixSliceImpl[V, +Coll<:Matrix[V]]
  (override val underlying : Coll, val keys1 : Seq[Int], val keys2 : Seq[Int])
  (implicit override val scalar : Scalar[V])
  extends MatrixSlice[V, Coll] {
    override def lookup1(i : Int) = keys1(i);
    override def lookup2(j : Int) = keys2(j);

    override val domain = TableDomain(keys1.length, keys2.length);
  }
}

trait MatrixCompanion[Bound[V]<:Matrix[V]]
extends tensor.MatrixCompanion[Bound] with IndexedTensorCompanion[(Int,Int),Bound] {
  /** Tighten bound on return value to be mutable. */
  override implicit def canMulMatrixByCol[V1,V2,RV]
  (implicit sr : CanSliceRow[Bound[V1],Int,tensor.VectorRow[V1]],
   mul : BinaryOp[tensor.VectorRow[V1],tensor.VectorCol[V2],OpMulRowVectorBy,RV],
   scalar : Scalar[RV])
  : BinaryOp[Bound[V1], tensor.VectorCol[V2], OpMulMatrixBy, VectorCol[RV]] =
  super.canMulMatrixByCol[V1,V2,RV](sr,mul,scalar).asInstanceOf[BinaryOp[Bound[V1], tensor.VectorCol[V2], OpMulMatrixBy, VectorCol[RV]]];

  /** Tighten bound on return value to be mutable. */
  override implicit def canMulMatrixByMatrix[V1,V2,RV]
  (implicit sr : CanSliceRow[Bound[V1],Int,tensor.VectorRow[V1]],
   sc : CanSliceCol[tensor.Matrix[V2],Int,tensor.VectorCol[V2]],
   mul : BinaryOp[tensor.VectorRow[V1],tensor.VectorCol[V2],OpMulRowVectorBy,RV],
   scalar : Scalar[RV])
  : BinaryOp[Bound[V1], tensor.Matrix[V2], OpMulMatrixBy, Matrix[RV]] =
  super.canMulMatrixByMatrix[V1,V2,RV](sr,sc,mul,scalar).asInstanceOf[BinaryOp[Bound[V1], tensor.Matrix[V2], OpMulMatrixBy, Matrix[RV]]];

//  /** Tighten bound on return value to be mutable. */
//  override implicit def canAppendMatrixColumns[V]
//  : CanAppendColumns[Bound[V],tensor.Matrix[V],Matrix[V]]
//  = super.canAppendMatrixColumns[V].asInstanceOf[CanAppendColumns[Bound[V],tensor.Matrix[V], Matrix[V]]];
//
//  /** Tighten bound on return value to be mutable. */
//  override implicit def canAppendVectorColumn[V]
//  : CanAppendColumns[Bound[V],tensor.VectorCol[V],Matrix[V]]
//  = super.canAppendVectorColumn[V].asInstanceOf[CanAppendColumns[Bound[V],tensor.VectorCol[V],Matrix[V]]];
}

