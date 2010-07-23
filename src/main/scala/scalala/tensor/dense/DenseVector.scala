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
package tensor.dense;

import collection.dense.{DenseMutableDomainSeqLike,DenseMutableDomainSeq};
import tensor.{VectorLike,Vector};

/**
 * A DenseVector is backed by an array of doubles.
 *
 * @author dramage
 */
trait DenseVectorLike[+This<:DenseVector]
extends DenseMutableDomainSeqLike[Double,This]
with VectorLike[This];

/**
 * A DenseVector is backed by an array of doubles.
 *
 * @author dramage
 */
class DenseVector(data : Array[Double])
extends DenseMutableDomainSeq[Double](data)
with Vector with DenseVectorLike[DenseVector] {
//  override def copy = new DenseVector(data.clone);
}

object DenseVector {
  /**
   * Static constructor that creates a dense vector of the given size
   * initialized by elements from the given values list (looping if
   * necessary).
   */
  def apply(size : Int)(values : Double*) =
    new DenseVector(Array.tabulate(size)(i => values(i % values.length)));
}
