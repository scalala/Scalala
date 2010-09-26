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

import domain.IndexDomain;

/**
 * A vector backed by a dense array.
 *
 * @author dramage
 */
class DenseVector[@specialized(Int,Long,Float,Double) B]
(override val data : Array[B])
(implicit override val scalar : Scalar[B])
extends DenseArrayTensor[Int,B] with DenseArrayTensorLike[Int,B,IndexDomain,DenseVector[B]]
with mutable.Vector[B] with mutable.VectorLike[B,DenseVector[B]] {
  override def size = data.length;

  override val domain = IndexDomain(data.length);

  override def apply(key : Int) =
    data(key);

  override def update(key : Int, value : B) =
    data(key) = value;

  override def foreachNonZero[U](fn : (B=>U)) =
    data.foreach(fn);

  override def foreachNonZero[U](fn : ((Int,B)=>U)) =
    this.foreach(fn);

  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transform(f : (Int,B)=>B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(i,data(i));
      i += 1;
    }
  }
  
  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transformValues(f : B=>B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(data(i));
      i += 1;
    }
  }
}

object DenseVector {
  /**
   * Static constructor that creates a dense vector of the given size
   * initialized by elements from the given values list (looping if
   * necessary).
   */
  def apply[B:Scalar:ClassManifest](size : Int)(values : B*) =
    new DenseVector(Array.tabulate(size)(i => values(i % values.length)));

  /** Tabulate a vector with the value at each offset given by the function. */
  def tabulate[B:Scalar:ClassManifest](size : Int)(f : (Int => B)) =
    new DenseVector(Array.tabulate(size)(f));

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
