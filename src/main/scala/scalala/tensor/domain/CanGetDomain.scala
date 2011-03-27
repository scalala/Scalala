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
 * Marker trait for statically getting the domain of a given tensor.
 *
 * @author dramage
 */
trait CanGetDomain[-From,Domain] extends (From => Domain);

trait CanGetDomainImplicitsLevel0 {
  implicit def domainForTensor[K,T](implicit view : T => Tensor[K,_])
  : CanGetDomain[T,IterableDomain[K]]
  = new CanGetDomain[T,IterableDomain[K]] { def apply(t : T) = t.domain; }
}

trait CanGetDomainImplicitsLevel1 extends CanGetDomainImplicitsLevel0 {
  implicit def domainForTensor1[K,T](implicit view : T => Tensor1[K,_])
  : CanGetDomain[T,Domain1[K]]
  = new CanGetDomain[T,Domain1[K]] { def apply(t : T) = t.domain; }

  implicit def domainForTensor2[K1,K2,T](implicit view : T => Tensor2[K1,K2,_])
  : CanGetDomain[T,Domain2[K1,K2]]
  = new CanGetDomain[T,Domain2[K1,K2]] { def apply(t : T) = t.domain; }
}

trait CanGetDomainImplicitsLevel2 extends CanGetDomainImplicitsLevel1 {
  implicit def domainForVector[T](implicit view : T => Vector[_])
  : CanGetDomain[T,IndexDomain]
  = new CanGetDomain[T,IndexDomain] { def apply(t : T) = t.domain; }
  
  implicit def domainForMatrix[T](implicit view : T => Matrix[_])
  = new CanGetDomain[T,TableDomain] { def apply(t : T) = t.domain; }
}

object CanGetDomain extends CanGetDomainImplicitsLevel2 {
  def apply[T] = new {
    def apply[D](implicit df : CanGetDomain[T,D]) = df;
  }
}

/**
 * Capability trait for statically getting the domain2 of a Tensor2.
 *
 * @author dramage
 */
trait CanGetDomain2[-From,D1,D2,Domain] extends (From => Domain) {
  def _1(from : From) : D1;
  def _2(from : From) : D2;
}

trait CanGetDomain2ImplicitsLevel0 {
  implicit def domainForTensor2[K1,K2,T](implicit view : T => Tensor2[K1,K2,_])
  : CanGetDomain2[T,Domain1[K1],Domain1[K2],Domain2[K1,K2]]
  = new CanGetDomain2[T,Domain1[K1],Domain1[K2],Domain2[K1,K2]] {
    def apply(t : T) = t.domain;
    def _1(t : T) = t.domain._1;
    def _2(t : T) = t.domain._2;
  }
}

trait CanGetDomain2ImplicitsLevel1 extends CanGetDomain2ImplicitsLevel0 {
  implicit def domainForMatrix[T](implicit view : T => Matrix[_])
  : CanGetDomain2[T,IndexDomain,IndexDomain,TableDomain]
  = new CanGetDomain2[T,IndexDomain,IndexDomain,TableDomain] {
    def apply(t : T) = t.domain;
    def _1(t : T) = t.domain._1;
    def _2(t : T) = t.domain._2;
  }
}

object CanGetDomain2 extends CanGetDomain2ImplicitsLevel1 {
  def apply[T] = new {
    def apply[D1,D2,D](implicit df : CanGetDomain2[T,D1,D2,D]) = df;
  }
}

