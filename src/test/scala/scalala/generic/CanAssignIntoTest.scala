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

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

import scalala.collection.domain._;

@RunWith(classOf[JUnitRunner])
class CanAssignIntoTest extends FunSuite with Checkers {

  import operators.Implicits._;

  test("Array[Int]") {
    val a = Array(0,0,0);
    val aRef = a;

    val b = Array(1,2,3);
    val bRef = b;

    a := b;
    assert(a.toList === b.toList);
    assert(!(a eq b));
    assert((a eq aRef));
    assert((b eq bRef));
  }

  test("Array[Array[Int]]") {
    val a = Array(Array(0,0),Array(0,0),Array(0,0));
    val aRef = a;

    val b = Array(Array(1,2),Array(3,4),Array(5,6));
    val bRef = b;

    a := b;
    assert(a.map(_.toList).toList === b.map(_.toList).toList);
    assert(!(a eq b));
    assert((a zip b).forall(tup => !(tup._1 eq tup._2)));
    assert((a eq aRef));
    assert((b eq bRef));
  }
}
