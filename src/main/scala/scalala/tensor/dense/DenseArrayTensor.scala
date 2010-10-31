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

import domain._;
import scalar.Scalar;

/**
 * Implementation trait for a tensor backed by a dense array of values.
 *
 * @author dramage
 */
trait DenseArrayTensorLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 +D<:IterableDomain[A] with DomainLike[A,D],
 +This<:DenseArrayTensor[A,B]]
extends mutable.TensorLike[A,B,D,This] {
  def data : Array[B];

  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2])
  : mutable.TensorBuilder[K2,V2,Tensor[K2,V2]] = domain match {
    case that : IndexDomain => DenseVector.zeros[V2](that.size).asBuilder;
    case that : TableDomain => DenseMatrix.zeros[V2](that.numRows, that.numCols).asBuilder;
    case _ => super.newBuilder[K2,V2](domain);
  }

  /** Assigns the given value to all elements of this map. */
  def :=(value : B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = value;
      i += 1;
    }
  }

  /** Tranforms all values in this map by applying the given function. */
  override def transformValues(f : B=>B) = {
    var i = 0;
    while (i < data.length) {
      data(i) = f(data(i));
      i += 1;
    }
  }

  override def foreachNonZeroValue[U](fn : (B=>U)) = {
    data.foreach(fn);
    true;
  }
}

/**
 * Mutable tensor backed by a dense array of values.
 *
 * @author dramage
 */
trait DenseArrayTensor
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
extends mutable.Tensor[A,B]
with DenseArrayTensorLike[A,B,IterableDomain[A],DenseArrayTensor[A,B]];
