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
package operators;

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

import Implicits._;

@RunWith(classOf[JUnitRunner])
class ArrayTest extends FunSuite with Checkers {

  def mk[V:ClassManifest](values : V*) =
    Array[V](values :_*);

  test("Negation") {
    val a = mk(1,2,3);
    assert((-a).toList === List(-1,-2,-3));
  }

  test("Collection Ops") {
    val a = mk(1,2,3);
    val b = mk(4,5,6);

    // TODO: file a bug report for this
    // assert((a :+ b).toList === List(5,7,9));
    assert((a :- b).toList === List(-3,-3,-3));
    assert((a :* b).toList === List(4,10,18));
    assert((b :/ a).toList === List(4,2,2));
    assert((a :/ b.map(_.toDouble)).toList === List(0.25,0.4,0.5));
    assert((b :% a).toList === List(0,1,0));
  }

  test("Scalar Ops") {
    val a = mk(1,2,3);

    // TODO: file a bug report for this
    // assert((a :+ 1.0).toList === List(2.0,3.0,4.0));
    assert((1.0 :+ a).toList === List(2.0,3.0,4.0));
    assert((a :- 1).toList === List(0,1,2));
    assert((a :* 3.0).toList === List(3.0,6.0,9.0));
    assert((a :/ 2.0).toList === List(0.5,1.0,1.5));
    assert((a :% 2).toList === List(1,0,1));
  }

  test("Vector ops") {
    val a = mk(1,2,3);

    assert(a.t * a === 1+4+9);
    assert((a * a.t).map(_.toList).toList === List(List(1,2,3),List(2,4,6),List(3,6,9)));
  }

  test("Matrix ops") {
    val m = Array(Array(1,2,4),Array(-1,7,0));
    val v = Array(6.0, 2.0, 1.0);
    assert((m * v).toList === Array(14,8).toList);

    assert((m * m.t).map(_.toList).toList === List(List(21,13), List(13,50)));
    assert((m.t * m).map(_.toList).toList === List(List(2,-5,4), List(-5,53,8), List(4,8,16)))
  }
}
