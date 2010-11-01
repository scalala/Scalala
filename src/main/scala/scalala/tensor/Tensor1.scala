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

import scalar.Scalar;

import domain._;
import generic.{CanAdd,CanMul};

import mutable.TensorBuilder;

/**
 * Implementation trait for a one-axis tensor supports methods like norm
 * and inner products (dot) with other one-axis tensors.
 *
 * @author dramage
 */
trait Tensor1Like
[@specialized(Int,Long)A, @specialized(Int,Long,Float,Double) B,
 +D<:IterableDomain[A] with DomainLike[A,D], +This<:Tensor1[A,B]]
extends TensorLike[A,B,D,This] { self =>

  /** Returns the k-norm of this tensor. */
  def norm(n : Double) : Double = {
    if (n == 1) {
      var sum = 0.0;
      foreachNonZeroValue(v => sum += scalar.norm(v));
      return sum;
    } else if (n == 2) {
      var sum = 0.0;
      foreachNonZeroValue(v => { val nn = scalar.norm(v); sum += nn * nn });
      return math.sqrt(sum);
    } else if (n == Double.PositiveInfinity) {
      var max = Double.NegativeInfinity;
      foreachNonZeroValue(v => { val nn = scalar.norm(v); if (nn > max) max = nn; });
      return max;
    } else {
      var sum = 0.0;
      foreachNonZeroValue(v => { val nn = scalar.norm(v); sum += math.pow(nn,n); });
      return math.pow(sum, 1.0 / n);
    }
  }

  /** Returns the inner product of this tensor with another. */
  def dot[C,R](that : Tensor1[A,C])(implicit mul : CanMul[B,C,R], add : CanAdd[R,R,R], scalar : Scalar[R]) : R = {
    checkDomain(that.domain);
    var sum = scalar.zero;
    foreachNonZero((k,v) => sum = add(sum, mul(v, that(k))));
    sum;
  }

  override protected def canEqual(other : Any) : Boolean = other match {
    case that : Tensor1[_,_] => true;
    case _ => false;
  }
}

/**
 * One-axis tensor supports methods like norm
 * and inner products (dot) with other one-axis tensors.
 *
 * @author dramage
 */
trait Tensor1[@specialized(Int,Long) A, @specialized(Int,Long,Float,Double) B]
extends Tensor[A,B] with Tensor1Like[A,B,IterableDomain[A],Tensor1[A,B]];

object Tensor1 extends Tensor1Companion[Tensor1] {
  def apply[K,V:Scalar](keys : (K,V)*) : Tensor1Col[K,V] = {
    val m = keys.toMap;
    val s = implicitly[Scalar[V]];
    new Tensor1Col[K,V] {
      override val scalar = s;
      override val domain = scalala.tensor.domain.SetDomain(m.keySet);
      override def apply(key : K) = m(key);
    }
  }
}

/**
 * Default companion for Tensor1.
 * 
 * @author dramage
 */
trait Tensor1Companion[Bound[K,V]<:Tensor1[K,V]] extends TensorCompanion[Bound];
