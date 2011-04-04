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
 * A domain that explicitly has only one element, i.e. A is not a tuple.
 *
 * @author dramage
 */
trait Domain1Like[@specialized(Int,Long) A, +This<:Domain1[A]]
extends IterableDomainLike[A,This] { outer =>

  /** Constructs the union of this and the other domain. */
  override def union(that : IterableDomain[A]) : IterableDomain[A] = that match {
    case d1 : Domain1[_] => this.union(d1.asInstanceOf[Domain1[A]]);
    case _ => super.union(that);
  }

  /** Constructs the union of this and the other domain. */
  def union(that : Domain1[A]) : Domain1[A] = {
    new Domain1[A] with UnionDomainLike[A,Domain1[A]] {
      override def a = outer.repr;
      override def b = that;
    }
  }

  def product[B,That<:Domain1[B]](that : That) =
    Domain2(repr,that);
}

/**
 * A domain that explicitly has only one element, i.e. A is not a tuple.
 *
 * @author dramage
 */
trait Domain1[@specialized(Int,Long) A]
extends IterableDomain[A] with Domain1Like[A,Domain1[A]];

