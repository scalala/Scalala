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
package scalala.scalar;

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ComplexTest extends FunSuite with Checkers {

  test("Add") {
    assert((1 + 2*i) + (2 + 3*i) === (3 + 5*i));
  }

  test("Sub") {
    assert((1 + 2*i) - (2 + 3*i) === (-1 - i));
  }

  test("Div") {
    assert((5 + 10*i) / (3 - 4*i) === (-1 + 2*i));
  }

  test("Mul") {
    assert((1 + 2*i) * (-3 + 6*i) === -15);
    assert((1 + 5*i) * (-3 + 2*i) === (-13 - 13*i));
  }

  test("Neg") {
    assert(-(1 + 2*i) === (-1 - 2*i));
  }

  test("Abs/Conj") {
    assert((3 + 4*i).abs === 5);
    val c = (1.7 + 2.1*i);
    assert(c * c.conjugate === 7.3);
  }
}
