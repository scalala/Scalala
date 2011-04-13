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
import scalala.generic.collection._
import operators.ValuesMonadic;

/**
 * Support for comprehensions on values from an underlying tensor.  This
 * class can be implicitly viewed as an Iterable[V].
 *
 * @author dramage
 */
trait TensorValuesMonadic
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +This<:Tensor[K,V]] extends ValuesMonadic[This,V] {
  
  /** Underlying tensor. */
  def repr : This;
  
  /** Calls repr.foreachValue. */
  def foreach[U](fn : V => U) =
    repr.foreachValue(fn);
  
  /** Calls repr.mapValues. */
  override def map[TT>:This,O,That](fn : V => O)
  (implicit bf : CanMapValues[TT, V, O, That]) : That =
    repr.mapValues(fn)(bf.asInstanceOf[CanMapValues[Tensor[K,V],V,O,That]]);

  /** Calls repr.valuesIterator. */
  def iterator =
    repr.valuesIterator;
  
  /** Constructs a filtered view of this tensor. */
  def filter[D,That](p : V => Boolean) =
    withFilter(p);
  
  /** Constructs a filtered view of this tensor. */
  def withFilter(p : V => Boolean) =
    new TensorValuesMonadic.Filtered[K,V,This](repr, p);

  def reduceLeft[B >: V](op: (B, V) => B): B = {
    var first = true
    var acc: B = repr.scalar.zero
    for (x <- this) {
      if (first) {
        acc = x
        first = false
      }
      else acc = op(acc, x)
    }
    acc
  }
}

object TensorValuesMonadic {
  /** Filtered view of the values in a Tensor.  Does not support map. */
  class Filtered
  [@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V, +This<:Tensor[K,V]]
  (val repr : This, p : V=>Boolean) {
    def foreach[U](fn : V => U) =
      repr.foreachValue(v => if (p(v)) fn(v));

    def withFilter(q : V => Boolean) =
      new Filtered[K,V,This](repr, v => p(v) && q(v));
    
//    def map[U,D,That](fn : V => U)
//    (implicit df : CanGetDomain[This,D], bf : CanBuildTensorFrom[This,D,K,U,That]) = {
//      val builder = bf(repr, repr.domain.asInstanceOf[D]);
//      repr.foreachPair((k,v) => if (p(v)) builder(k) = fn(v));
//      builder.result;
//    }
//    
//    def strict[D,That]
//    (implicit df : CanGetDomain[This,D], bf : CanBuildTensorFrom[This,D,K,V,That]) = {
//      val builder = bf(repr, repr.domain.asInstanceOf[D]);
//      repr.foreachPair((k,v) => if (p(v)) builder(k) = v);
//      builder.result;
//    }
  }
  
  implicit def asIterable[K, @specialized(Int,Long,Float,Double) V, T<:Tensor[K,V]]
  (values : TensorValuesMonadic[K,V,T]) = {
    new Iterable[V] {
      def self = values.repr;
      override def foreach[U](fn : V => U) = self.foreachValue(fn);
      override def iterator = self.valuesIterator;
    }
  }
}

