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
package domain;

/**
 * Implementation trait for a domain containing all pairs from
 * two underlying domains.
 *
 * @author dramage
 */
trait Domain2Like
[@specialized(Int,Long) A1, @specialized(Int,Long) A2,
 +D1 <: Domain1[A1] with Domain1Like[A1,D1],
 +D2 <: Domain1[A2] with Domain1Like[A2,D2],
 +Transpose <: Domain2Like[A2,A1,D2,D1,This,Transpose],
 +This <: Domain2Like[A1,A2,D1,D2,Transpose,This]]
extends IterableDomain[(A1,A2)] with DomainLike[(A1,A2),This] {
  /** Row-space domain. */
  def _1 : D1;

  /** Col-space domain. */
  def _2 : D2;

  /** Constructs the union of this and the other domain. */
  override def union(that : IterableDomain[(A1,A2)]) : IterableDomain[(A1,A2)] = that match {
    case d1 : Domain2[_,_] => {
      val casted = d1.asInstanceOf[Domain2[A1,A2]];
      Domain2(this._1 union casted._1, this._2 union casted._2);
    }
    case _ => super.union(that);
  }

  /** Returns the transpose of this domain. */
  def transpose : Transpose;

  override def foreach[O](fn : ((A1,A2)) => O) =
    for (k1 <- _1.iterator; k2 <- _2.iterator) fn((k1,k2));

  /** Iterators all elements of this domain. */
  override def iterator =
    for (k1 <- _1.iterator; k2 <- _2.iterator) yield (k1,k2);

  /** Returns true if a1 is in the row space and a2 is in the col space. */
  def contains(a1 : A1, a2 : A2) =
    _1.contains(a1) && _2.contains(a2);

  /** Defers to contains(tup._1, tup._2). */
  final override def contains(tup : (A1,A2)) =
    contains(tup._1, tup._2);

  override def equals(other : Any) = other match {
    case that : Domain2[_,_] =>
      this._1 == that._1 && this._2 == that._2;
    case _ => super.equals(other);
  }
}

/**
 * All pairs of elements from D1 and D2, both iterable domains.
 *
 * @author dramage
 */
trait Domain2
[@specialized(Int,Long) A1, @specialized(Int,Long) A2]
extends Product2[Domain1[A1],Domain1[A2]] with IterableDomain[(A1,A2)]
with Domain2Like[A1,A2,Domain1[A1],Domain1[A2],Domain2[A2,A1],Domain2[A1,A2]] {

  def transpose =
    Domain2[A2,A1](_2,_1);

  override def size =
    _1.size * _2.size;

  override def canEqual(that : Any) =
    that.isInstanceOf[Domain2[_,_]];

  override def toString =
    "Domain2("+_1.toString+","+_2.toString+")";
}

object Domain2 {
  def apply[A1,A2](d1 : Domain1[A1], d2 : Domain1[A2])
  : Domain2[A1,A2] = new Impl(d1,d2);

  class Impl[@specialized(Int,Long) A1, @specialized(Int,Long) A2]
  (override val _1 : Domain1[A1], override val _2 : Domain1[A2])
  extends Domain2[A1,A2];
}

