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
  val TOLERANCE = 1e-4;
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE);

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

  test("MulInner") {
    val a = mutable.Counter(1->0.56390,2->0.36231,3->0.14601,4->0.60294,5->0.14535);
    val b = mutable.Counter(1->0.15951,2->0.83671,3->0.56002,4->0.57797,5->0.54450);
    assertClose(a dot b, .90249);
  }

  test("Zero + non zero is nonzero") {
    val a = mutable.Counter[Int,Double](1->0.0)
    val b = mutable.Counter(1->0.15951);
    assert(a + b === b, (a + b).toString + " not equal " + b)
  }

  test("Mean") {
    assert(Counter(0->0,1->1,2->2).mean === 1.0);
    assert(Counter(0->0.0,1->3.0).mean === 1.5);
    assert(Counter(0->3l).mean === 3.0);
  }

  test("assignment checks both domains") {
    val a = Counter[Int,Int]()
    val b = Counter[Int,Int](3->4)
    a := b
    assert(a === b)
  }
}

