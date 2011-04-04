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
package scalala.tensor;

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class CounterTest extends FunSuite with Checkers {

  test("Addition") {
    assert(mutable.Counter("a"->1,"b"->2) + mutable.Counter("a"->3) === Counter("a"->4,"b"->2));
    assert(mutable.Counter("a"->3) + mutable.Counter("a"->1,"b"->2) === Counter("a"->4,"b"->2));
  }

  test("Subtraction") {
    assert(mutable.Counter("a"->1,"b"->2) - mutable.Counter("a"->3) === Counter("a" -> -2, "b" -> 2));
    assert(mutable.Counter("a"->3) - mutable.Counter("a"->1,"b"->2) === Counter("a" -> 2, "b" -> -2));
  }

  test("Multiplication") {
    assert(mutable.Counter("a"->1,"b"->2) :* mutable.Counter("a"->3) === Counter("a"->3));
    assert(mutable.Counter("a"->3) :* mutable.Counter("a"->1,"b"->2) === Counter("a"->3));
  }
}

