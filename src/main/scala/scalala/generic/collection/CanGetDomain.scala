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
package generic;
package collection;

import scalala.tensor.domain._;

/**
 * Marker for being able to get the domain (keys) of a collection.
 *
 * @author dramage
 */
trait CanGetDomain[-Coll, @specialized(Int,Long) K] {
  def apply(coll : Coll) : Domain[K];
}

object CanGetDomain {
  implicit object OpArray extends CanGetDomain[Array[_],Int] {
    def apply(v : Array[_]) = IndexDomain(v.length);
  }

  implicit def opMap[K,V] = new OpMap[K,V];

  class OpMap[K,V] extends CanGetDomain[scala.collection.Map[K,V],K] {
    def apply(v : scala.collection.Map[K,V]) = SetDomain(v.keySet);
  }
}

/**
 * Marker for being able to get the domain (keys) of a collection.
 *
 * @author dramage
 */
trait CanGetProduct2Domain[-Coll, @specialized(Int,Long) K1, @specialized(Int,Long) K2] {
  def apply(coll : Coll) : Product2Domain[K1,K2]
}
