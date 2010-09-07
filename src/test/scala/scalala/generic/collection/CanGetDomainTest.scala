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

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

import scalala.collection.domain._;

@RunWith(classOf[JUnitRunner])
class CanGetDomainTest extends FunSuite with Checkers {
  def domain[V,K](v : V)(implicit domain : CanGetDomain[V,K]) : Domain[K] =
    domain(v);

  test("array") {
    assert(domain(Array(1.0,2.0,3.0)) === IndexDomain(3));
  }

  test("map") {
    assert(domain(Map('a'->2,'b'->3)) === SetDomain(Set('a','b')));
  }
}
