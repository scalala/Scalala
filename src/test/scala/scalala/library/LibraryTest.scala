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
package library;

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class LibraryTest extends FunSuite with Checkers {
  test("log") {
    assert(Library.log(2.8) === 1.0296194171811581);
    assert(Library.log(1) === 0);
    assert(Library.log(Array(1,2,3,4)).toList === List(0.0, 0.6931471805599453, 1.0986122886681098, 1.3862943611198906))
  }

  test("mean") {
    // assert(Library.mean(List(1,2,3,4)) === ((1+2+3+4) / 4));
    assert(Library.mean(List(Array(0,2),Array(2,4))).toList === List(1,3));
    assert(Library.mean(List(Array(1.0,3.0),Array(2.0,4.0))).toList === List(1.5,3.5));
  }

  test("exp") {
    assert(Library.exp(Array(1,2,3,4)).toList === List(1,2,3,4).map(_.toDouble).map(math.exp));
  }
}
