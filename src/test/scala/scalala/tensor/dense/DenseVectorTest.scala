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
package tensor;
package dense;

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class DenseVectorTest extends FunSuite with Checkers {

  val TOLERANCE = 1e-4;
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE);

  test("Min/Max") {
    val v = DenseVector(5)(2,0,3,2,-1);
    assert(v.argmin === 4);
    assert(v.argmax === 2);
    assert(v.min === -1);
    assert(v.max === 3);
  }

  test("Norm") {
    val v = DenseVector(5)(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    assertClose(v.norm(1), 3.6577);
    assertClose(v.norm(2), 2.0915);
    assertClose(v.norm(3), 1.8405);
    assertClose(v.norm(4), 1.7541);
    assertClose(v.norm(5), 1.7146);
    assertClose(v.norm(6), 1.6940);
    assertClose(v.norm(Double.PositiveInfinity), 1.6656);
  }

  test("Dot") {
    val a = DenseVector(5)(0.56390,0.36231,0.14601,0.60294,0.14535);
    val b = DenseVector(5)(0.15951,0.83671,0.56002,0.57797,0.54450);
    assertClose(a dot b, .90249);
  }

//  test("Map") {
//    val a : DenseVector[Int] = DenseVector(5)(1,2,3,4,5);
//    val m : DenseVector[Int] = a.mapValues(_ + 1);
//    assert(m.data.toList === List(2,3,4,5,6));
//  }

  test("Tabulate") {
    val m = DenseVector.tabulate(5)(i => i + 1);
    assert(m.data.toList === List(1,2,3,4,5));
  }
}
