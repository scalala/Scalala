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
 * Implementation trait for a domain that can be traversed.  Default
 * equality check returns true if this eq other or if this and other
 * traverse the same elements in the same order.
 *
 * @author dramage
 */
trait IterableDomainLike[@specialized(Int,Long) A, +This<:IterableDomain[A]]
extends DomainLike[A,This] { outer =>
  /** Applies the given function to every element of the domain. */
  def foreach[O](f : A => O);

  /** Iterates over the elements of the domain. */
  def iterator : Iterator[A];

  /** Constructs the union of this and the other domain. */
  def union(that : IterableDomain[A]) : IterableDomain[A] = {
    new IterableDomain[A] with UnionDomainLike[A,IterableDomain[A]] {
      override def a = outer.repr;
      override def b = that;
    }
  }

  /** Number of elements in the domain. */
  def size : Int;

  def toIndexedSeq =
    iterator.toIndexedSeq;

  def toArray(implicit mf : Manifest[A]) =
    iterator.toArray;

  def filter(fn : A => Boolean) : List[A] =
    iterator.filter(fn).toList;

  def max(implicit ord : Ordering[A]) =
    iterator.max(ord);

  def min(implicit ord : Ordering[A]) =
    iterator.min(ord);

  override def equals(other : Any) = other match {
    case that : IterableDomain[_] =>
      (this eq that) || (this.iterator zip that.iterator).forall(tup => tup._1 == tup._2);
    case _ =>
      false;
  }
}

/**
 * A domain that can be traversed.
 *
 * @author dramage
 */
trait IterableDomain[@specialized(Int,Long) A]
extends Domain[A] with IterableDomainLike[A,IterableDomain[A]];

