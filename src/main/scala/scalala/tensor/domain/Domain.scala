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
 * Implementation trait for domains of a DomainFunction, representing
 * a restriction on values of type A.
 *
 * @author dramage
 */
trait DomainLike[@specialized(Int,Long,Float,Double) A, +This<:Domain[A]]
extends (A => Boolean) {

  def repr : This =
    this.asInstanceOf[This];

  /** Calls contains(key). */
  final override def apply(key : A) = contains(key);

  /** Returns true if the given element is part of the set. */
  def contains(key : A) : Boolean;
}

/**
 * Domains of a DomainFunction, representing a restriction on values of type A.
 *
 * @author dramage
 */
trait Domain[@specialized(Int,Long,Float,Double) A]
extends DomainLike[A, Domain[A]];

/**
 * An exception thrown when encountering an invalid domain.
 *
 * @author dramage
 */
class DomainException(msg : String) extends RuntimeException(msg) {
  def this() = this(null);
}

