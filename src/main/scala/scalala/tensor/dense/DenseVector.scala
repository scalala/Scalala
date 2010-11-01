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
package dense;

import domain.{IterableDomain,IndexDomain};

import generic.collection.CanTranspose;
import scalar.Scalar;

/**
 * A vector backed by a dense array.
 *
 * @author dramage
 */
class DenseVector[@specialized(Int,Long,Float,Double) V]
(override val data : Array[V])
(implicit override val scalar : Scalar[V])
extends mutable.Vector[V] with mutable.VectorLike[V,DenseVector[V]]
with DenseArrayTensor[Int,V] with DenseArrayTensorLike[Int,V,IndexDomain,DenseVector[V]] { self =>

  override def size = data.length;

  override val domain = IndexDomain(data.length);

  override def apply(key : Int) =
    data(key);

  override def update(key : Int, value : V) =
    data(key) = value;

  override def foreach[U](fn : ((Int,V)=>U)) = {
    var i = 0;
    while (i < data.length) {
      fn(i,data(i));
      i += 1;
    }
  }

  override def foreachValue[U](fn : (V=>U)) =
    data.foreach(fn);

  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transform(fn : (Int,V)=>V) = {
    var i = 0;
    while (i < data.length) {
      data(i) = fn(i,data(i));
      i += 1;
    }
  }
  
  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transformValues(fn : V=>V) = {
    var i = 0;
    while (i < data.length) {
      data(i) = fn(data(i));
      i += 1;
    }
  }
}

object DenseVector extends mutable.VectorCompanion[DenseVector] with DenseVectorConstructors {

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

/**
 * Constructors for dense vectors.
 *
 * @author dramage
 */
trait DenseVectorConstructors {
  /** Constructs a DenseVector for the given IndexDomain. */
  def apply[S:Scalar](domain : IndexDomain) =
    zeros(domain.size);

  /** Constructs a literal DenseVector. */
  def apply[V:Scalar](values : V*) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseVectorCol(values.toArray);
  }

  /** Dense vector containing the given value for all elements. */
  def fill[V:Scalar](size : Int)(value : V) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseVectorCol(Array.fill(size)(value));
  }

  /** Dense vector of zeros of the given size. */
  def zeros[V:Scalar](size : Int) =
    fill(size)(implicitly[Scalar[V]].zero);

  /** Dense vector of ones of the given size. */
  def ones[V:Scalar](size : Int) =
    fill(size)(implicitly[Scalar[V]].one);

  /** Tabulate a vector with the value at each offset given by the function. */
  def tabulate[V:Scalar](size : Int)(f : (Int => V)) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseVectorCol(Array.tabulate(size)(f));
  }
}

/**
 * DenseVectors as a row.
 *
 * @author dramage
 */
class DenseVectorRow[@specialized(Int,Long,Float,Double) V]
(override val data : Array[V])
(implicit override val scalar : Scalar[V])
extends DenseVector[V](data)(scalar) with mutable.VectorRow[V] with mutable.VectorRowLike[V,DenseVectorRow[V]] {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    domain match {
      case that : IndexDomain => new DenseVectorRow(new Array[V2](size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
}

object DenseVectorRow extends mutable.VectorRowCompanion[DenseVectorRow] {
  /** Transpose shares the same data. */
  implicit def canTranspose[V] : CanTranspose[DenseVectorRow[V],DenseVectorCol[V]]
  = new CanTranspose[DenseVectorRow[V],DenseVectorCol[V]] {
    override def apply(row : DenseVectorRow[V]) =
      new DenseVectorCol(row.data)(row.scalar);
  }
}

/**
 * DenseVectors as a column.
 *
 * @author dramage
 */
class DenseVectorCol[@specialized(Int,Long,Float,Double) V]
(override val data : Array[V])
(implicit override val scalar : Scalar[V])
extends DenseVector[V](data)(scalar) with mutable.VectorCol[V] with mutable.VectorColLike[V,DenseVectorCol[V]]  {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    domain match {
      case that : IndexDomain => new DenseVectorCol(new Array[V2](size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
}

object DenseVectorCol extends mutable.VectorColCompanion[DenseVectorCol] {
  /** Transpose shares the same data. */
  implicit def canTranspose[V] : CanTranspose[DenseVectorCol[V],DenseVectorRow[V]]
  = new CanTranspose[DenseVectorCol[V],DenseVectorRow[V]] {
    override def apply(row : DenseVectorCol[V]) =
      new DenseVectorRow(row.data)(row.scalar);
  }
}
