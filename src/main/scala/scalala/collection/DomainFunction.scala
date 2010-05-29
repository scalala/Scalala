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
package collection;

import domain._;
import generic._;

/**
 * A function over a Domain of elements of type A returning values
 * of type B.  This type extends PartialFunction by restricting the
 * domain of possible input functions to an explicitly represented domain.
 *
 * @author dramage
 */
trait DomainFunction
[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) A,
 @specialized(scala.Int, scala.Long, scala.Float, scala.Double, scala.Boolean) +B,
 D <: Domain[A]]
extends PartialFunction[A, B] {
  
  /** The domain over which this function is defined. */
  def domain : D;

  /** Returns true if the key is in the function's domain. */
  override def isDefinedAt(key : A) =
    domain(key);

  /** @throws DomainException if key is not in the domain. */
  protected def checkKey(key : A) : Unit = {
    if (!isDefinedAt(key))
      throw new DomainException("Key " + key + " not in domain");
  }

  /** @throws DomainException if domain is not equal to this domain. */
  protected def checkDomain(domain : Domain[A]) : Unit = {
    if (this.domain != domain)
      throw new DomainException("Incompatible domain: "+domain);
  }

  def apply(key : A) : B;
}

//trait CanBuildDomainFunction[-From, @specialized A, @specialized B, D<:Domain[A], +To] {
//  def apply(from : From) : To;
//}
//
//object CanBuildDomainFunction {
//  implicit def fromFunction[@specialized A, @specialized B, D<:Domain[A]]
//  (implicit db : CanBuildTypeDomain[A,D]) =
//  new CanBuildDomainFunction[(A=>B),A,B,D,DomainFunction[A,B,D]] {
//    override def apply(function : (A => B)) {
//      val _domain = db.domain;
//      new DomainFunction[A,B,D] {
//        override def domain = _domain;
//        override def apply(x : A) = function(x)
//      }
//    }
//  }
//}

//object DomainFunction {
//
////  implicit def iMakeDomainFunction[From,@specialized A,@specialized B, D<:Domain[A],To]
////  (from : From)(implicit bf : CanBuildDomainFunction[From,A,B,D,To]) =
////    bf(from);
//
//  /**
//   * Converts a regular function into a domain function by assuming the
//   * full TypeDomain for the given input type.
//   */
//  implicit def apply[@specialized A,@specialized B,D<:TypeDomain[A]]
//  (function : (A => B))(implicit db : CanBuildTypeDomain[A,D]) = {
//    val _domain = db.domain;
//    new DomainFunction[A,B,D] {
//      override def domain = _domain;
//      override def apply(x : A) = function(x)
//    }
//  }
//}
