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
trait Tensor1Like
[@specialized(Int,Long)A, +D<:IterableDomain[A] with DomainLike[A,D],
 +This<:Tensor1[A]]
extends TensorLike[A,D,This] {
  /** Returns the k-norm of this tensor.  Calls scalala.Scalala.norm(this). */
  def norm(n : Double) : Double = {
    if (n == 1) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += math.abs(v));
      return sum;
    } else if (n == 2) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += v * v);
      return math.sqrt(sum);
    } else if (n % 2 == 0) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += math.pow(v,n));
      return math.pow(sum, 1.0 / n);
    } else if (n % 2 == 1) {
      var sum = 0.0;
      valuesIterator.foreach(v => sum += math.pow(math.abs(v),n));
      return math.pow(sum, 1.0 / n);
    } else if (n == Double.PositiveInfinity) {
      var max = Double.NegativeInfinity;
      valuesIterator.foreach(v => { val av = math.abs(v); if (av > max) max = av; });
      return max;
    } else {
      throw new UnsupportedOperationException();
    }
  }

  /** Returns the inner product of this tensor with another. */
  def dot(that : Tensor1[A]) : Double = {
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
trait Tensor1[@specialized(Int,Long) A]
extends Tensor[A] with Tensor1Like[A,IterableDomain[A],Tensor1[A]];


object Tensor1 {
//  implicit def canSliceFrom[A1, A2, D<:IterableDomain[A1] with DomainLike[A1,D]] =
//  new DomainMapCanSliceFrom[Tensor1[A1,Double,D], A1, D, A2, Double, Tensor1[A1,Double,SetDomain[A2]]] {
//    override def apply(from : Tensor1[A1,Double,D], keymap : scala.collection.Map[A2,A1]) =
//      new MutableDomainMapSlice.FromKeyMap[A1, D, A2, B, MutableDomainMap[A1,B,D]](from, keymap);
//  }
}
