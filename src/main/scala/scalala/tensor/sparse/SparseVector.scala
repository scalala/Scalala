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
package sparse;

import domain.{IterableDomain,IndexDomain};
import generic.{CanMul,CanMulColumnBy,CanMulRowBy};
import generic.collection.{CanSliceCol,CanTranspose,CanAppendColumns};

import scalala.collection.sparse.{SparseArray,DefaultArrayValue};
import scalala.scalar.Scalar;

/**
 * A vector backed by a SparseArray.
 *
 * @author dramage
 */
@serializable
@SerialVersionUID(1)
trait SparseVector[@specialized(Int,Long,Float,Double) B]
extends SparseArrayTensor[Int,B] with SparseArrayTensorLike[Int,B,IndexDomain,SparseVector[B]]
with mutable.Vector[B] with mutable.VectorLike[B,SparseVector[B]] {
  override def size = data.length;

  override val domain = IndexDomain(data.length);

  override def apply(key : Int) =
    data(key);

  override def update(key : Int, value : B) =
    data(key) = value;

  override def foreachNonZero[U](fn : ((Int,B)=>U)) = {
    data.foreachActive(fn);
    data.activeLength == data.length;
  }

  override def foreachNonZeroValue[U](fn : (B=>U)) = {
    data.foreachActive(fn);
    data.activeLength == data.length;
  }
}

object SparseVector extends mutable.VectorCompanion[SparseVector] {
  /** Creates a sparse vector literal. */
  def apply[B:Scalar:ClassManifest:DefaultArrayValue](values : B*) =
    new SparseVectorCol(SparseArray(values :_*))

  /** Returns a vector of zeros of the given size. */
  def zeros[B:Scalar:ClassManifest:DefaultArrayValue](size : Int) =
    create(size)();

  /** Creates a sparse vector of the given size with given initial values. */
  def create[B:Scalar:ClassManifest:DefaultArrayValue](size : Int)(values : (Int,B)*) =
    new SparseVectorCol(SparseArray.create(size)(values :_*));

  /** Tabulate a vector with the value at each offset given by the function. */
  def tabulate[B:Scalar:ClassManifest:DefaultArrayValue](size : Int)(f : (Int => B)) =
    new SparseVectorCol(SparseArray.tabulate(size)(f));
}

class SparseVectorRow[@specialized(Int,Long,Float,Double) V]
(override val data : SparseArray[V])
(implicit override val scalar : Scalar[V])
extends SparseVector[V] with mutable.VectorRow[V] with mutable.VectorRowLike[V,SparseVectorRow[V]] {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    implicit val dv = implicitly[Scalar[V2]].defaultArrayValue;
    domain match {
      case that : IndexDomain => new SparseVectorRow(new SparseArray[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
}

object SparseVectorRow extends mutable.VectorRowCompanion[SparseVectorRow] {
  /** Transpose shares the same data. */
  implicit def canTranspose[V] : CanTranspose[SparseVectorRow[V],SparseVectorCol[V]]
  = new CanTranspose[SparseVectorRow[V],SparseVectorCol[V]] {
    override def apply(row : SparseVectorRow[V]) =
      new SparseVectorCol(row.data)(row.scalar);
  }

  /** Tighten bound on super to be sparse in return value. */
  override implicit def canMulVectorRowByMatrix[V1,V2,Col,RV]
  (implicit slice : CanSliceCol[Matrix[V2],Int,Col], mul : CanMulRowBy[SparseVectorRow[V1],Col,RV], scalar : Scalar[RV]) =
     super.canMulVectorRowByMatrix[V1,V2,Col,RV](slice,mul,scalar).asInstanceOf[CanMulRowBy[SparseVectorRow[V1],tensor.Matrix[V2],SparseVectorRow[RV]]];
}


/**
 * SparseVectors as a column.
 *
 * @author dramage
 */
class SparseVectorCol[@specialized(Int,Long,Float,Double) V]
(override val data : SparseArray[V])
(implicit override val scalar : Scalar[V])
extends SparseVector[V] with mutable.VectorCol[V] with mutable.VectorColLike[V,SparseVectorCol[V]]  {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    implicit val dv = implicitly[Scalar[V2]].defaultArrayValue;
    domain match {
      case that : IndexDomain => new SparseVectorCol(new SparseArray[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
}

object SparseVectorCol extends mutable.VectorColCompanion[SparseVectorCol] {
  /** Transpose shares the same data. */
  implicit def canTranspose[V] : CanTranspose[SparseVectorCol[V],SparseVectorRow[V]]
  = new CanTranspose[SparseVectorCol[V],SparseVectorRow[V]] {
    override def apply(row : SparseVectorCol[V]) =
      new SparseVectorRow(row.data)(row.scalar);
  }

//  /** Tighten bound on super to be a Sparse in return value. */
//  override implicit def canMulVectorColByRow[V1,V2,RV](implicit mul : CanMul[V1,V2,RV], scalar : Scalar[RV])
//  = super.canMulVectorColByRow[V1,V2,RV](mul, scalar).asInstanceOf[CanMulColumnBy[SparseVectorCol[V1],tensor.VectorRow[V2],SparseMatrix[RV]]];
//
//  /** Tighten bound on super to be a Sparse in return value. */
//  override implicit def canAppendMatrixColumns[V]
//  : CanAppendColumns[SparseVectorCol[V],tensor.Matrix[V],SparseMatrix[V]]
//  = super.canAppendMatrixColumns[V].asInstanceOf[CanAppendColumns[SparseVectorCol[V],tensor.Matrix[V], SparseMatrix[V]]];
//
//  /** Tighten bound on super to be a Sparse in return value. */
//  override implicit def canAppendVectorColumn[V]
//  : CanAppendColumns[SparseVectorCol[V],tensor.VectorCol[V],SparseMatrix[V]]
//  = super.canAppendVectorColumn[V].asInstanceOf[CanAppendColumns[SparseVectorCol[V],tensor.VectorCol[V],SparseMatrix[V]]];
}

