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
 * Marker trait for statically getting the Domain2 for two
 * input domains.
 *
 * @author dramage
 */
trait CanGetDomain2For[-A,-B,RV] extends ((A,B) => RV);

trait CanGetDomain2ForImplicitsLevel0 {
  implicit def domainForGeneral[K1,K2]
  : CanGetDomain2For[Domain1[K1],Domain1[K2],Domain2[K1,K2]]
  = new CanGetDomain2For[Domain1[K1],Domain1[K2],Domain2[K1,K2]] {
    def apply(a : Domain1[K1], b : Domain1[K2]) =
      a.product[K2,Domain1[K2]](b);
  }
}

trait CanGetDomain2ForImplicitsLevel1 extends CanGetDomain2ForImplicitsLevel0 {
  implicit object DomainForIndexIndex 
  extends CanGetDomain2For[IndexDomain,IndexDomain,TableDomain] {
    override def apply(a : IndexDomain, b : IndexDomain) =
      TableDomain(a.size, b.size);
  }
}

object CanGetDomain2For extends CanGetDomain2ForImplicitsLevel1;

