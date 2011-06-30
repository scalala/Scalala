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
package mutable;

import domain._;

import scalala.scalar.Scalar;
import scalala.generic.collection._
import tensor.Counter2.Curried
import scala.collection.mutable.HashMap
;

/**
 * A mutable tensor that acts like a collection of key1-key2-value pairs backed by
 * a map of maps.
 *
 * @author dlwh,dramage
 */
trait Counter2Like
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V,
 +M1[VV]<:Curried[scala.collection.mutable.Map,K1]#Result[VV],
 +T<:scalala.tensor.mutable.Counter[K2,V],
 +This<:Counter2[K1,K2,V]]
extends tensor.Counter2Like[K1,K2,V,M1,T,This] with Tensor2Like[K1,K2,V,SetDomain[K1], SetDomain[K2], Domain2[K1,K2], Domain2[K2,K1], This] { self =>

  def update(k1 : K1, k2: K2, v : V) =
    innerGetOrElseUpdate(k1,data)(k2) = v;

  private[mutable] def innerGetOrElseUpdate[M](k:K1, m: scala.collection.mutable.Map[K1,M]): M = {
    m.getOrElseUpdate(k,m.default(k))
  }
}

trait Counter2
[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V]
extends tensor.Counter2[K1,K2,V] with Tensor2[K1,K2,V]
with Counter2Like[K1,K2, V,Curried[scala.collection.mutable.Map,K1]#Result,Counter[K2,V],Counter2[K1,K2,V]];

object Counter2 {
  @serializable
  class Impl[K1, @specialized(Int,Long) K2, @specialized(Int,Long,Float,Double) V]
  (override val data : scala.collection.mutable.Map[K1,Counter[K2,V]])
  (implicit override val scalar : Scalar[V])
  extends Counter2[K1,K2,V];

  /** Returns a new empty counter. */
  def apply[K1,K2,V:Scalar]() : Counter2[K1,K2,V] = {
    val map = new HashMap[K1,Counter[K2,V]] {
      override def default(k: K1) = Counter[K2,V]();
    }
    new Impl[K1,K2,V](map);
  }
    
  /** Aggregates the counts in the given items. */
  def apply[K1,K2,V:Scalar](values : (K1,K2,V)*) : Counter2[K1,K2,V] =
    apply(values);

  /** Aggregates the counts in the given items. */
  def apply[K1,K2,V:Scalar](values : TraversableOnce[(K1,K2,V)]) : Counter2[K1,K2,V] = {
    val rv = apply[K1,K2,V]();
    values.foreach({ case (k1,k2,v) => rv(k1,k2) = rv.scalar.+(rv(k1,k2), v) });
    rv;
  }

  /** Constructs a Counter2 -- currently ignores the domain. */
  def apply[K1,K2,V:Scalar](domain : Domain2[K1,K2]) : Counter2[K1,K2,V] =
    apply[K1,K2,V]();
  
  /** Counts the given elements. */
  def count[K1,K2](values : TraversableOnce[(K1,K2)]) : Counter2[K1,K2,Int] = {
    val rv = apply[K1,K2,Int]();
    values.foreach({ case (k1,k2) => rv(k1,k2) += 1; });
    rv;
  }

  implicit def canSliceRow[K1,K2,V](implicit vS : Scalar[V])
  : CanSliceRow[Counter2[K1,K2,V],K1,Counter[K2,V]]
  = new CanSliceRow[Counter2[K1,K2,V],K1,Counter[K2,V]] {
    override def apply(from : Counter2[K1,K2,V], row : K1) = from.innerGetOrElseUpdate(row,from.data);
  }
  
  implicit def canSliceCol[K1,K2,V](implicit vS : Scalar[V])
  : CanSliceCol[Counter2[K1,K2,V],K2,Counter[K1,V]]
  = new CanSliceCol[Counter2[K1,K2,V],K2,Counter[K1,V]] {
    override def apply(from : Counter2[K1,K2,V], col : K2) = new Counter[K1,V] {
      override val data = new scala.collection.mutable.Map[K1,V] {
        override def apply(k1 : K1) =
          from(k1,col);
          
        override def update(k1 : K1, v : V) =
          from(k1,col) = v;
          
        override def -=(k1 : K1) = {
          from.data(k1)(col) = vS.zero;
          this;
        }
        
        override def +=(tup : (K1,V)) = {
          from.data(tup._1)(col) = (tup._2);
          this;
        }
        
        override def iterator =
          for ((k1,map) <- from.data.iterator) yield (k1,map(col));
          
        override def get(k1 : K1) =
          from.data.get(k1).map(_(col));
          
        override def keySet = from.data.keySet;
        
        override def size = from.data.size;
      }
      implicit val scalar = vS;
    }
  }
}

