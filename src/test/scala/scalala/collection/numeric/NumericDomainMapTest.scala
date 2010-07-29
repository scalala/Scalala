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
package numeric;

import generic._;
import domain._;

import org.scalatest.FunSuite;


trait NumericDomainMapTest[A,B,D<:IterableDomain[A] with DomainLike[A,D],M<:NumericDomainMap[A,B,D]]
extends FunSuite {

  /** Create a static example map. */
  def createExample : M;

  /** Gold standard min and max */
  def exampleMinMax : (B,B);

  /** Gold standard argmin and argmax */
  def exampleArgMinArgMax : (A,A);

//  implicit def canMapValues[That] : DomainMapCanMapValuesFrom[M,A,B,B,D,That];

  test("Min/Max") {
    val v = createExample;
    assert(v.argmin === exampleArgMinArgMax._1);
    assert(v.argmax === exampleArgMinArgMax._2);
    assert(v.min === exampleMinMax._1);
    assert(v.max === exampleMinMax._2);
  }

//  test("MapValues") {
//    val a = createExample;
//    val b = a.mapValues(x => a.numeric.+(x,a.numeric.one));
//    for (k <- a.domain) {
//      assert(a.numeric.+(a(k), a.numeric.one) === b(k));
//    }
//  }
}
