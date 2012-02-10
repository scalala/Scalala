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


import scalala.collection.sparse.{SparseArray,DefaultArrayValue};
import scalala.scalar.Scalar;

import scalala.operators._
import scalala.generic.collection.{CanCopy, CanCreateZerosLike, CanSliceCol, CanAppendColumns}
;

/**
 * A vector backed by a SparseArray.
 *
 * @author dramage
 */
@serializable
@SerialVersionUID(1)
trait SparseVector[@specialized(Int,Long,Float,Double) V]
extends SparseArrayTensor[Int,V] with SparseArrayTensorLike[Int,V,IndexDomain,SparseVector[V]]
with mutable.Vector[V] with mutable.VectorLike[V,SparseVector[V]] {
  override def length = data.length;

  override def apply(key : Int) =
    data(key);

  override def update(key : Int, value : V) =
    data(key) = value;

  override def foreachNonZeroKey[U](fn : (Int=>U)) = {
    data.foreachActiveKey(fn);
    data.activeLength == data.length;
  }

  override def foreachNonZeroValue[U](fn : (V=>U)) = {
    data.foreachActiveValue(fn);
    data.activeLength == data.length;
  }

  override def foreachNonZeroPair[U](fn : ((Int,V)=>U)) = {
    data.foreachActivePair(fn);
    data.activeLength == data.length;
  }
}

object SparseVector {
  /** Creates a sparse vector literal. */
  def apply[V:Scalar:ClassManifest:DefaultArrayValue](values : V*) =
    new SparseVectorCol(SparseArray(values :_*))

  /** Returns a vector of zeros of the given size. */
  def zeros[V:Scalar:ClassManifest:DefaultArrayValue](size : Int) =
    create(size)();

  /** Creates a sparse vector of the given size with given initial values. */
  def create[V:Scalar:ClassManifest:DefaultArrayValue](size : Int)(values : (Int,V)*) =
    new SparseVectorCol(SparseArray.create(size)(values :_*));

  /** Tabulate a vector with the value at each offset given by the function. */
  def tabulate[V:Scalar:ClassManifest:DefaultArrayValue](size : Int)(f : (Int => V)) =
    new SparseVectorCol(SparseArray.tabulate(size)(f));

    /** Optimized base class for creating zeros */
  trait CanCreateZerosSparseVector
  [@specialized V, @specialized RV, DV[V]<:SparseVector[V]]
    extends CanCreateZerosLike[DV[V],DV[RV]] {
    def create(length : Int) : DV[RV];
    def apply(v1: DV[V]) = create(v1.length);
  }


  class GenericSparseVectorRowBase[@specialized V:Scalar:Manifest:DefaultArrayValue] {
    def create(length : Int) = new SparseVectorRow(new SparseArray[V](length))
  }

  class GenericSparseVectorColBase[@specialized V:Scalar:Manifest:DefaultArrayValue] {
    def create(length : Int) = new SparseVectorCol(new SparseArray[V](length))
  }

  class GenericSparseVectorBase[@specialized V:Scalar:Manifest:DefaultArrayValue] {
    def create(length : Int) = new SparseVectorCol(new SparseArray[V](length))
  }


  implicit def canCreateZerosSparseVector[@specialized V, @specialized RV:Scalar:Manifest:DefaultArrayValue]
  : CanCreateZerosSparseVector[V, RV, SparseVector] =
    new GenericSparseVectorColBase with CanCreateZerosSparseVector[V, RV, SparseVector];

    /** Optimized base class for mapping dense columns. */
  implicit def canCreateZerosSparseVectorCols[@specialized V, @specialized RV:Scalar:Manifest:DefaultArrayValue]
  : CanCreateZerosSparseVector[V, RV, SparseVectorCol] =
  new GenericSparseVectorColBase with CanCreateZerosSparseVector[V, RV, SparseVectorCol];

  /** Optimized base class for mapping dense rows. */
  implicit def canCreateZerosSparseVectorRows[@specialized V, @specialized RV:Scalar:Manifest:DefaultArrayValue]
  : CanCreateZerosSparseVector[V, RV, SparseVectorRow] =
  new GenericSparseVectorRowBase with CanCreateZerosSparseVector[V, RV, SparseVectorRow];


  /** Optimized base class for mapping sparse columns. */
  implicit def canCopySparseVectorCols[@specialized V:Scalar:Manifest] = new CanCopySparseVectorCol[V];

  /** Optimized base class for mapping sparse rows. */
  implicit def canCopySparseVectorRows[@specialized V:Scalar:Manifest] = new CanCopySparseVectorRow[V];

  /** Optimized base class for mapping sparse . */
  implicit def canCopySparseVector[@specialized V:Scalar:Manifest] = new CanCopySparseVector[V];

  /** Optimized base class for copying sparse */
  class CanCopySparseVectorRow[@specialized V:Scalar:ClassManifest] extends CanCopy[SparseVectorRow[V]] {
    def apply(v1: SparseVectorRow[V]) = {
      new SparseVectorRow[V](v1.data.copy)
    }
  }

  class CanCopySparseVector[@specialized V:Scalar:ClassManifest] extends CanCopy[SparseVector[V]] {
    def apply(v1: SparseVector[V]) = {
      new SparseVectorCol[V](v1.data.copy)
    }
  }

  class CanCopySparseVectorCol[@specialized V:Scalar:ClassManifest] extends CanCopy[SparseVectorCol[V]] {
    def apply(v1: SparseVectorCol[V]) = {
      new SparseVectorCol[V](v1.data.copy)
    }
  }
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
  
  override def t : SparseVectorCol[V] =
    new SparseVectorCol(data)(scalar);
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
  
  override def t : SparseVectorRow[V] =
    new SparseVectorRow(data)(scalar);
}

