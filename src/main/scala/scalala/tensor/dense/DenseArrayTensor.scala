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

import domain.{IterableDomain,IndexDomain,TableDomain};
import generic.TensorBuilder;

import scalala.scalar.Scalar;
import scalala.generic.collection._;
import scalala.operators._;

/**
 * Implementation trait for a tensor backed by a dense array of values.
 *
 * @author dramage
 */
trait DenseArrayTensorLike
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V,
 +D<:IterableDomain[K],
 +This<:DenseArrayTensor[K,V]]
extends mutable.TensorLike[K,V,D,This] {
  def data : Array[V];

  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2])
  : TensorBuilder[K2,V2,Tensor[K2,V2]] = domain match {
    case that : IndexDomain => DenseVector.zeros[V2](that.size).asBuilder;
    case that : TableDomain => DenseMatrix.zeros[V2](that.numRows, that.numCols).asBuilder;
    case _ => super.newBuilder[K2,V2](domain);
  }

  override def foreachNonZeroValue[U](fn : (V=>U)) = {
    foreachValue(fn);
    true;
  }
}

/**
 * Mutable tensor backed by a dense array of values.
 *
 * @author dramage
 */
trait DenseArrayTensor
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double,Boolean) V]
extends mutable.Tensor[K,V]
with DenseArrayTensorLike[K,V,IterableDomain[K],DenseArrayTensor[K,V]];
