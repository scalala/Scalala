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

import domain.{DomainLike,IterableDomain};

import scalala.collection.sparse.SparseArray;
import scalala.scalar.Scalar;

/**
 * Implementation trait for a tensor backed by a sparse array of values.
 *
 * @author dramage
 */
trait SparseArrayTensorLike
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B,
 +D<:IterableDomain[A] with DomainLike[A,D],
 +This<:SparseArrayTensor[A,B]]
extends mutable.TensorLike[A,B,D,This] {
  def data : SparseArray[B];

  /** Assigns the given value to all elements of this map. */
  def :=(value : B) = {
    if (value == data.default) {
      data.clear;
    } else {
      var i = 0;
      while (i < data.length) {
        data(i) = value;
        i += 1;
      }
    }
  }

  /** Tranforms all values in this map by applying the given function. */
  override def transformValues(f : B=>B) =
    data.transform(f);
}

/**
 * Mutable tensor by a sparse array of values.
 *
 * @author dramage
 */
trait SparseArrayTensor
[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double,Boolean) B]
extends mutable.Tensor[A,B]
with SparseArrayTensorLike[A,B,IterableDomain[A],SparseArrayTensor[A,B]];
