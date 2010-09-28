/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala;
package tensor;

import domain._;

/**
 * A function over a Domain of elements of type A returning values
 * of type B.  This type extends PartialFunction by restricting the
 * domain of possible input functions to an explicitly represented domain.
 *
 * @author dramage
 */
trait DomainFunction
[@specialized(Int,Long,Float,Double) A,
 @specialized(Int,Long,Float,Double,Boolean) +B,
 +D <: Domain[A]]
extends PartialFunction[A, B] {
  
  /** The domain over which this function is defined. */
  def domain : D;

  /** Returns true if the key is in the function's domain. */
  override def isDefinedAt(key : A) =
    domain(key);

  /** @throws DomainException if key is not in the domain. */
  def checkKey(key : A) : Unit = {
    if (!isDefinedAt(key))
      throw new DomainException("Key " + key + " not in domain");
  }

  /** @throws DomainException if domain is not equal to this domain. */
  def checkDomain(domain : Domain[A]) : Unit = {
    if (this.domain != domain)
      throw new DomainException("Incompatible domain: "+domain);
  }

  def apply(key : A) : B;
}
