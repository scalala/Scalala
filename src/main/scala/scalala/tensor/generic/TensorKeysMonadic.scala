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
 * Support for comprehensions on keys from an underlying tensor.  This
 * class can be implicitly viewed as an Iterable[K].
 *
 * @author dramage
 */
trait TensorKeysMonadic
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +This<:Tensor[K,V]] {
  
  def repr : This;
  
  /** Calls repr.foreachKey. */
  def foreach[U](fn : K => U) =
    repr.foreachKey(fn);

  /** Calls repr.size. */
  def size =
    repr.size;

  /** Calls repr.keysIterator. */
  def iterator =
    repr.keysIterator;
  
  /** Constructs a filtered view of this tensor. */
  def filter[D,That](p : K => Boolean) =
    withFilter(p);
  
  /** Constructs a filtered view of this tensor. */
  def withFilter(p : K => Boolean) =
    new TensorKeysMonadic.Filtered[K,V,This](repr, p);
}

object TensorKeysMonadic {
  /** Filtered view of the keys in a Tensor.  Does not support map. */
  class Filtered
  [@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V, +This<:Tensor[K,V]]
  (val repr : This, p : K=>Boolean) {
    def foreach[U](fn : K => U) =
      repr.foreachKey(k => if (p(k)) fn(k));
    
    def withFilter(q : K => Boolean) =
      new Filtered[K,V,This](repr, k => p(k) && q(k));
    
//    def map[U,D,That](fn : K => U)
//    (implicit df : CanGetDomain[This,D], bf : CanBuildTensorFrom[This,D,K,U,That]) = {
//      val builder = bf(repr, repr.domain.asInstanceOf[D]);
//      repr.foreachPair((k,v) => if (p(k)) builder(k) = fn(k));
//      builder.result;
//    }
//      
//    def strict[D,That]
//    (implicit df : CanGetDomain[This,D], bf : CanBuildTensorFrom[This,D,K,V,That]) = {
//      val builder = bf(repr, repr.domain.asInstanceOf[D]);
//      repr.foreachPair((k,v) => if (p(k)) builder(k) = v);
//      builder.result;
//    }
  }
  
  implicit def asIterable[K, V, T<:Tensor[K,V]](values : TensorKeysMonadic[K,V,T]) = {
    new Iterable[K] {
      def self = values.repr;
      override def foreach[U](fn : K => U) = self.foreachKey(fn);
      override def iterator = self.keysIterator;
    }
  }
}

