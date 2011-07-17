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

import scalala.generic.collection.{CanSliceCol,CanAppendColumns};
import scalala.collection.sparse.{SparseArray,DefaultArrayValue};
import scalala.scalar.Scalar;

import scalala.operators._;

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

