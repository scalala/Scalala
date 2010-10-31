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

import domain.IndexDomain;

import scalala.collection.sparse.{SparseArray,DefaultArrayValue};
import scalala.scalar.Scalar;

/**
 * A vector backed by a SparseArray.
 *
 * @author dramage
 */
class SparseVector[@specialized(Int,Long,Float,Double) B]
(override val data : SparseArray[B])
(implicit override val scalar : Scalar[B])
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
  def apply[B:Scalar:ClassManifest:DefaultArrayValue](size : Int)(values : (Int,B)*) =
    new SparseVector(SparseArray.create(size)(values :_*));

  /** Tabulate a vector with the value at each offset given by the function. */
  def tabulate[B:Scalar:ClassManifest:DefaultArrayValue](size : Int)(f : (Int => B)) =
    new SparseVector(SparseArray.tabulate(size)(f));

//  implicit object DenseVectorCanMapValuesFrom
//  extends DomainMapCanMapValuesFrom[DenseVector,Int,Double,Double,DenseVector] {
//    override def apply(from : DenseVector, fn : (Double=>Double)) = {
//      val data = new Array[Double](from.size);
//      var i = 0;
//      while (i < data.length) {
//        data(i) = fn(from.data(i));
//        i += 1;
//      }
//      new DenseVector(data);
//    }
//
//    override def apply(from : DenseVector, fn : ((Int,Double)=>Double)) = {
//      val data = new Array[Double](from.size);
//      var i = 0;
//      while (i < data.length) {
//        data(i) = fn(i, from.data(i));
//        i += 1;
//      }
//      new DenseVector(data);
//    }
//  }
}
