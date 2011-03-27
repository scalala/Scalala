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
 * Marker trait for statically getting the domain of a given object.
 *
 * @author dramage
 */
trait DomainFor[-From,Domain];

trait DomainForImplicitsLevel0 {
  implicit def domainForTensor[K,T](implicit view : T => Tensor[K,_])
  : DomainFor[T,IterableDomain[K]] = null;
}

trait DomainForImplicitsLevel1 extends DomainForImplicitsLevel0 {
  implicit def domainForTensor1[K,T](implicit view : T => Tensor1[K,_])
  : DomainFor[T,Domain1[K]] = null;

  implicit def domainForTensor2[K1,K2,T](implicit view : T => Tensor2[K1,K2,_])
  : DomainFor[T,Domain2[K1,K2]] = null;
}

object DomainFor extends DomainForImplicitsLevel1 {
  implicit def domainForVector[T](implicit view : T => Vector[_])
  : DomainFor[T,IndexDomain] = null;
  
  implicit def domainForMatrix[T](implicit view : T => Matrix[_])
  : DomainFor[T,TableDomain] = null;
  
  def apply[T] = new {
    def apply[D](implicit df : DomainFor[T,D]) = df;
  }
}

