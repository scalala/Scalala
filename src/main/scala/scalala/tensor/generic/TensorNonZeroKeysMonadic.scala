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
package generic;

import domain.CanGetDomain;
import scalala.generic.collection._;

/**
 * Support for comprehensions on all non-zero-valued keys (and some zeros) from
 * an underlying tensor.  This class can be implicitly viewed as an Iterable[K].
 *
 * @author dramage
 */
trait TensorNonZeroKeysMonadic
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +This<:Tensor[K,V]] {
  
  def repr : This;
  
  /** Calls repr.foreachKey. */
  def foreach[U](fn : K => U) =
    repr.foreachNonZeroKey(fn);

  /** Calls repr.nonzeroSize. */
  def size =
    repr.nonzeroSize;

  /** Calls repr.keysIterator. */
  def iterator =
    repr.keysIteratorNonZero;
  
  /** Constructs a filtered view of this tensor. */
  def filter[D,That](p : K => Boolean) =
    withFilter(p);
  
  /** Constructs a filtered view of this tensor. */
  def withFilter(p : K => Boolean) =
    new TensorNonZeroKeysMonadic.Filtered[K,V,This](repr, p);
}

object TensorNonZeroKeysMonadic {
  /** Filtered view of the keys in a Tensor.  Does not support map. */
  class Filtered
  [@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V, +This<:Tensor[K,V]]
  (val repr : This, p : K=>Boolean) {
    def foreach[U](fn : K => U) =
      repr.foreachNonZeroKey(k => if (p(k)) fn(k));
    
    def withFilter(q : K => Boolean) =
      new Filtered[K,V,This](repr, k => p(k) && q(k));
  }
  
  implicit def asIterable[K, V, T<:Tensor[K,V]](values : TensorNonZeroKeysMonadic[K,V,T]) = {
    new Iterable[K] {
      def self = values.repr;
      override def foreach[U](fn : K => U) = self.foreachNonZeroKey(fn);
      override def iterator = self.keysIteratorNonZero;
    }
  }
}

