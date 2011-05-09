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
import mutable.Tensor;

@RunWith(classOf[JUnitRunner])
class DenseVectorTest extends FunSuite with Checkers {

  val TOLERANCE = 1e-4;
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE);

  test("Min/Max") {
    val v = DenseVector(2,0,3,2,-1);
    assert(v.argmin === 4);
    assert(v.argmax === 2);
    assert(v.min === -1);
    assert(v.max === 3);
  }

  test("Mean") {
    assert(DenseVector(0,1,2).mean === 1.0);
    assert(DenseVector(0.0,3.0).mean === 1.5);
    assert(DenseVector(3l).mean === 3.0);
  }

  test("Norm") {
    val v = DenseVector(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    assertClose(v.norm(1), 3.6577);
    assertClose(v.norm(2), 2.0915);
    assertClose(v.norm(3), 1.8405);
    assertClose(v.norm(4), 1.7541);
    assertClose(v.norm(5), 1.7146);
    assertClose(v.norm(6), 1.6940);
    assertClose(v.norm(Double.PositiveInfinity), 1.6656);
  }

  test("MulInner") {
    val a = DenseVector(0.56390,0.36231,0.14601,0.60294,0.14535);
    val b = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450);
    assertClose(a dot b, .90249);
    assertClose(a.t * b, .90249);
  }

  test("MulOuter") {
    val a = DenseVector(1, 2, 3);
    val b = DenseVector(6, -4, 8);

    // assert result is a dense matrix
    val m : DenseMatrix[Int] = a * b.t;
    assert(m === DenseMatrix((6,-4,8),(12,-8,16),(18,-12,24)));
  }

  test("MulMatrix") {
    val x = DenseVector(1,2).t;
    val m = DenseMatrix((1,2,1),(2,7,8));

    // assert return type is statically a dense row
    val r : DenseVectorRow[Int] = x * m;
    assert(r === DenseVector(5, 16, 17));
  }

  test("Slice") {
    val x = DenseVector.zeros[Int](5);

    // check that the slice is a vector and mutable
    val y : mutable.Vector[Int] = x(0 to 2);
    y :+= 1;

    val z : mutable.Vector[Int] = x(1 to 3);
    z :+= 1;

    assert(x === DenseVector(1,2,2,1,0));

    assert(x(0 until 5) === x);
    assert(try { x(0 to 5); false; } catch { case _ => true });
  }

  test("Slice and Transpose") {
    val x = DenseVector[Int](1, 2, 3, 4, 5);

    val s: DenseVectorCol[Int] = x(2 to 3);

    assert(s === DenseVector(3, 4));

    val t: mutable.Vector[Int] = s.t;

    assert(t === DenseVector(3, 4).t);
  }

  // https://github.com/scalala/Scalala/pull/19
  test("Supertype mutator") {
    val a = DenseVector(13, 42);
    val b = DenseVector(7, 42);
    val expected = DenseVector(6, 0);
  
    val direct = a - b;
    assert(direct === expected);
  
    (a: Tensor[Int, Int]) :-= b;
    assert(a === expected);
  }

  test("Transpose") {
    val x : DenseVectorCol[Int] = DenseVector(1,2,3);

    // test static type and write-through of transpose
    val y : DenseVectorRow[Int] = x.t;
    y(0) = 0;
    assert(x === DenseVector(0,2,3));
  }

  test("MapValues") {
    val a : DenseVector[Int] = DenseVector(1,2,3,4,5);
    val m : DenseVector[Int] = a.mapValues(_ + 1);
    assert(m === DenseVector(2,3,4,5,6));
  }

  test("ForComprehensions") {
    val a : DenseVectorCol[Int] = DenseVector(1,2,3,4,5);

    var s = 0;

    // foreach
    s = 0;
    for (v <- a) s += v;
    assert(s === a.sum);

    // filter
    s = 0;
    for (v <- a; if v < 3) s += v;
    assert(s === 1+2);

    // map
    val b1 : DenseVectorCol[Double] = for (v <- a) yield v * 2.0;
    assert(b1 === DenseVector(2.0,4.0,6.0,8.0,10.0));

    // map with filter
    val b2 : DenseVectorCol[Int] = for (v <- a; if v < 3) yield v * 2;
    assert(b2 === DenseVector(2.0,4.0));

    // transpose map with filter
    val b3 : DenseVectorRow[Int] = for (v <- a.t; if v < 3) yield v * 3;
    assert(b3 === DenseVector(3,6).t);
  }

  test("Tabulate") {
    val m = DenseVector.tabulate(5)(i => i + 1);
    assert(m === DenseVector(1,2,3,4,5));
  }

  test("VertCat") {
    val a1 = DenseVector(1,2,3);
    val a2 = DenseVector(2,3,4);
    val res = DenseVector(1,2,3,2,3,4);
    assert(DenseVector.vertcat(a1,a2) === res);
  }

  test("Negation") {
    val a1 = DenseVector(1,2,3);
    assert(-a1 == DenseVector(-1,-2,-3))

  }
}
