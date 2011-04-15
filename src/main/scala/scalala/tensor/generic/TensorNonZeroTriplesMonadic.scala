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
import scala.collection.generic.CanBuildFrom
import operators.HasValuesMonadic

/**
 * For comprehensions on nonzero triples of values from an underlying 2-axis tensor.
 * This class can be implicitly viewed as an Iterable[(K1,K2,V)].
 *
 * @author dramage
 */
trait TensorNonZeroTriplesMonadic
[@specialized(Int,Long) K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V,
 +This<:Tensor2[K1,K2,V]] {

  /** Underlying tensor. */
  def repr : This;
  
  /** Calls repr.foreachTriple. */
  def foreach[U](fn : ((K1,K2,V)) => U) =
    repr.foreachNonZeroTriple((k1,k2,v) => fn((k1,k2,v)));

  /** Calls repr.size. */
  def size =
    repr.size;
  
  /** Calls repr.mapTriples. */
  def map[TT>:This,O,That](fn : ((K1,K2,V)) => O)
  (implicit bf : CanMapKeyValuePairs[TT, (K1,K2), V, O, That]) : That =
    repr.mapNonZeroTriples((k1,k2,v) => fn((k1,k2,v)))(bf.asInstanceOf[CanMapKeyValuePairs[Tensor2[K1,K2,V],(K1,K2),V,O,That]]);

  /** Calls repr.TriplesIterator. */
  def iterator =
    repr.triplesIteratorNonZero;

  /** Constructs a filtered view of this tensor. */
  def filter(p : ((K1,K2,V)) => Boolean) =
    withFilter(p);
  
  /** Constructs a filtered view of this tensor. */
  def withFilter(p : ((K1,K2,V)) => Boolean) =
    new TensorNonZeroTriplesMonadic.Filtered[K1,K2,V,This](repr, p);
}

object TensorNonZeroTriplesMonadic {
  /** Filtered view of the Triples in a Tensor.  Does not support map. */
  class Filtered
  [@specialized(Int,Long) K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V, +This<:Tensor2[K1,K2,V]]
  (val repr : This, p : ((K1,K2,V))=>Boolean) {
    def foreach[U](fn : ((K1,K2,V)) => U) =
      repr.foreachNonZeroTriple((k1,k2,v) => if (p((k1,k2,v))) fn((k1,k2,v)));
    
    def withFilter(q : ((K1,K2,V)) => Boolean) =
      new Filtered[K1,K2,V,This](repr, tup => p(tup) && q(tup));

     /** Calls repr.mapTriples. */
    def map[O,That](fn : ((K1,K2,V)) => O)
    (implicit bf : CanBuildFrom[Iterable[(K1,K2,V)],O,That]) : That = {
      val b = bf();
      repr.foreachNonZeroTriple((k1,k2,v) => if(p((k1,k2,v))) b += fn((k1,k2,v)));
      b.result;
    }
  }
  
  implicit def asIterable[K1, K2, V, T<:Tensor2[K1,K2,V]](values : TensorTriplesMonadic[K1,K2,V,T]) = {
    new Iterable[(K1,K2,V)] {
      def self = values.repr;
      override def foreach[U](fn : ((K1,K2,V)) => U) =
        self.foreachNonZeroTriple((i,j,v) => fn((i,j,v)));
      override def iterator = self.triplesIteratorNonZero;
    }
  }
}

