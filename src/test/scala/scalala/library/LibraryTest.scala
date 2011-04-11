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
import tensor.dense.DenseVector

@RunWith(classOf[JUnitRunner])
class LibraryTest extends FunSuite with Checkers {
  test("log") {
    assert(Library.log(2.8) === 1.0296194171811581);
    assert(Library.log(1) === 0);
    assert(Library.log(Array(1,2,3,4)).toList === List(0.0, 0.6931471805599453, 1.0986122886681098, 1.3862943611198906))
  }

  test("mean") {
    assert(Library.mean(List(Array(0,2),Array(2,4))).toList === List(1,3));
    assert(Library.mean(List(Array(1.0,3.0),Array(2.0,4.0))).toList === List(1.5,3.5));
    assert(Library.mean(Iterator(1,2,3)) === 2.0);
    assert(Library.mean(List(1,2,3,4)) === ((1+2+3+4) / 4.0));
  }

  test("stddev") {
    assert(Library.stddev(Array(1,2,3)) === 1.0);
    assert(Library.stddev(Iterator(1,2,3,4)) === 1.2909944487358056);
  }

  test("exp") {
    assert(Library.exp(Array(1,2,3,4)).toList === List(1,2,3,4).map(_.toDouble).map(math.exp));
  }

  test("Tensor:Norm") {
    def assertEquals(a : Double, b : Double, t : Double) =
      if (math.abs(a - b) > t) throw new AssertionError;

    val v = Array(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    assertEquals(Library.norm(v,1), 3.6577, 1e-4);
    assertEquals(Library.norm(v,2), 2.0915, 1e-4);
    assertEquals(Library.norm(v,3), 1.8405, 1e-4);
    assertEquals(Library.norm(v,4), 1.7541, 1e-4);
    assertEquals(Library.norm(v,5), 1.7146, 1e-4);
    assertEquals(Library.norm(v,6), 1.6940, 1e-4);
    assertEquals(Library.norm(v,Double.PositiveInfinity), 1.6656, 1e-4);
  }

  test("Tensor:Normalize") {
    def assertEquals(a : Double, b : Double, t : Double) =
      if (math.abs(a - b) > t) throw new AssertionError;

    val v = DenseVector(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    val norm2V = Library.normalize(v,2);
    val norm1V = Library.normalize(v,1);
    assertEquals(Library.norm(norm2V,2), 1.0, 1e-4);
    assertEquals(Library.norm(norm1V,1), 1.0, 1e-4);
  }

   test("Tensor:LogNormalize") {
    def assertEquals(a : Double, b : Double, t : Double) =
      if (math.abs(a - b) > t) throw new AssertionError;

    val v = DenseVector(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    val normed = Library.logNormalize(v);
    assertEquals(Library.softmax(normed), 0.0, 1e-4);
  }
}
