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
class DenseMatrixTest extends FunSuite with Checkers {

  test("Slicing") {
    val m = DenseMatrix.zeros[Int](2,3);

    // the slice should be mutable
    val s : mutable.Matrix[Int] = m((0 to 1), (1 to 2));

    // update slices
    m((0 to 1), (1 to 2)) := 1;
    m((0 to 1), (0 to 1)) :+= 4;

    // check write-through
    assert(m.data.toList === List(4, 4, 5, 5, 1, 1));

    // check column slice.  should be mutable and write-through.
    val x : mutable.VectorCol[Int] = m(::, 1);
    assert(x === DenseVector(4,4))
    x(1) = 7;
    assert(m(1,1) === 7);

    // check row slice.  should be mutable and write-through.
    val y : mutable.VectorRow[Int] = m(0, ::);
    assert(y === DenseVector(4,5,1).t);
    y(0) = -1;
    assert(m(0,0) === -1);
  }

  test("Transpose") {
    val m = DenseMatrix((1,2,3),(4,5,6));

    // check that the double transpose gives us back the original
    assert(m.t.t eq m);

    // check static type and write-through
    val t : mutable.Matrix[Int] = m.t;
    assert(t === DenseMatrix((1,4),(2,3),(5,6)));
    t(0,0) = 0;
    assert(m === DenseMatrix((1,2,3),(4,5,6)));
  }

  test("Min/Max") {
    val m = DenseMatrix((1,0,0),(2,3,-1));
    assert(m.argmin === (1,2));
    assert(m.argmax === (1,1));
    assert(m.min === -1);
    assert(m.max === 3);
  }

  test("Map") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,0),(2,3,-1));
    val m : DenseMatrix[Int] = a.mapValues(_ + 1);
    assert(m.data.toList === List(2,3,1,4,1,0));
  }

  test("Tabulate") {
    val m = DenseMatrix.tabulate(2,2)((i,j) => 1.0 * (i+1) * (j+2));
    assert(m(0,0) === 2);
    assert(m(0,1) === 3);
    assert(m(1,0) === 4);
    assert(m(1,1) === 6);
  }

  test("Multiply") {
    val a = DenseMatrix((1, 2, 3),(4, 5, 6));
    val b = DenseMatrix((7, -2, 8),(-3, -3, 1),(12, 0, 5));
    val c = DenseVector(6,2,3);
    assert(a * b === DenseMatrix((37, -8, 25), (85, -23, 67)));
    assert(a * c === DenseVector(19,52));
  }
}
