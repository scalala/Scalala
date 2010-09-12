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
class SparseArrayTest extends FunSuite with Checkers {

  import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

  def mk[V:ClassManifest:DefaultArrayValue](values : V*) =
    SparseArray(values :_*);

  test("Negation") {
    val a = mk(1,0,3);
    assert((-a).toList === List(-1,0,-3));
  }

  test("Collection Ops") {
    val a = mk(1,2,3);
    val b = mk(4,5,6);

    assert((a :+: b).toList === List(5,7,9));
    assert((a :- b).toList === List(-3,-3,-3));
    assert((a :* b).toList === List(4,10,18));
    assert((b :/ a).toList === List(4,2,2));
    assert((a :/ b.map(_.toDouble)).toList === List(0.25,0.4,0.5));
    assert((b :% a).toList === List(0,1,0));
  }

  test("Scalar Ops") {
    val a = mk(1,2,3);

    assert((a :+: 1.0).toList === List(2.0,3.0,4.0));
    assert((1.0 :+: a).toList === List(2.0,3.0,4.0));
    assert((a :- 1).toList === List(0,1,2));
    assert((a :* 3.0).toList === List(3.0,6.0,9.0));
    assert((a :/ 2.0).toList === List(0.5,1.0,1.5));
    assert((a :% 2).toList === List(1,0,1));
  }
}
