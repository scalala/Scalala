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
 * Marker trait for statically getting the Product2Domain for two
 * input domains.
 *
 * @author dramage
 */
trait CanGetProduct2DomainFor[-A,-B,RV] extends ((A,B) => RV);

trait CanGetProduct2DomainForImplicitsLevel0 {
  implicit def domainForGeneral[K1,K2]
  : CanGetProduct2DomainFor[IterableDomain[K1],IterableDomain[K2],Product2Domain[K1,K2]]
  = new CanGetProduct2DomainFor[IterableDomain[K1],IterableDomain[K2],Product2Domain[K1,K2]] {
    def apply(a : IterableDomain[K1], b : IterableDomain[K2]) =
      a.product[K2,IterableDomain[K2]](b);
  }
}

trait CanGetProduct2DomainForImplicitsLevel1 extends CanGetProduct2DomainForImplicitsLevel0 {
  implicit object DomainForIndexIndex 
  extends CanGetProduct2DomainFor[IndexDomain,IndexDomain,TableDomain] {
    override def apply(a : IndexDomain, b : IndexDomain) =
      TableDomain(a.size, b.size);
  }
}

object CanGetProduct2DomainFor extends CanGetProduct2DomainForImplicitsLevel1;

