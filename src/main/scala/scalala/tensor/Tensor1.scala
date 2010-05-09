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

import collection._;
import collection.domain._;

/**
 * Implementation trait for a one-axis tensor supports methods like norm
 * and inner products (dot) with other one-axis tensors.
 *
 * @author dramage
 */
trait Tensor1Like[A,D<:IterableDomain[A],+This]
extends TensorLike[A,D,This] {
  /** Returns the k-norm of this tensor.  Calls scalala.Scalala.norm(this). */
  def norm(n : Double) : Double = {
    if (n == 1) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += Math.abs(v));
      return sum;
    } else if (n == 2) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += v * v);
      return Math.sqrt(sum);
    } else if (n % 2 == 0) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += Math.pow(v,n));
      return Math.pow(sum, 1.0 / n);
    } else if (n % 2 == 1) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += Math.pow(Math.abs(v),n));
      return Math.pow(sum, 1.0 / n);
    } else if (n == Double.PositiveInfinity) {
      var max = 0.0;
      valuesIterator.foreach(v => { val av = Math.abs(v); if (av > max) max = av; });
      return max;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  /** Returns the inner product of this tensor with another. */
  def dot(that : Tensor1[A,D]) : Double = {
    checkDomain(that.domain);
    var sum = 0.0;
    foreach((k,v) => sum += v * that(k));
    sum;
  }
}

/**
 * One-axis tensor supports methods like norm
 * and inner products (dot) with other one-axis tensors.
 *
 * @author dramage
 */
trait Tensor1[A,D<:IterableDomain[A]]
extends Tensor[A,D] with Tensor1Like[A,D,Tensor1[A,D]];
