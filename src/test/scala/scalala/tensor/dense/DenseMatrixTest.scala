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
    val m = DenseMatrix((0,1,2),
                        (3,4,5));
    
    // slice sub-matrix
    val s1 : mutable.Matrix[Int] = m(0 to 1, 1 to 2);
    assert(s1 === DenseMatrix((1,2),(4,5)));
    s1 += 1;
    assert(m === DenseMatrix((0,2,3),(3,5,6)));
    
    // slice row
    val s2 : mutable.VectorRow[Int] = m(0, ::);
    assert(s2 === DenseVector(0,2,3));
    s2 *= 2;
    assert(m === DenseMatrix((0,4,6),(3,5,6)));
    
    // slice column
    val s3 : mutable.VectorCol[Int] = m(::, 1);
    assert(s3 === DenseVector(4,5));
    s3 -= 1;
    assert(m === DenseMatrix((0,3,6),(3,4,6)));
    
    // slice rows
    val s4 : mutable.Matrix[Int] = m(1 to 1, ::);
    assert(s4 === DenseMatrix((3,4,6)));
    
    // slice columns
    val s5 : mutable.Matrix[Int] = m(::, 1 to 2);
    assert(s5 === DenseMatrix((3,6),(4,6)));
    
    // slice part of a row
    m(0, 1 to 2) += 1;
    assert(m === DenseMatrix((0,4,7),(3,4,6)));
    
    // slice part of a column
    m(0 to 1, 0) += 3;
    assert(m === DenseMatrix((3,4,7),(6,4,6)));
  }

  test("Transpose") {
    val m = DenseMatrix((1,2,3),(4,5,6));

    // check that the double transpose gives us back the original
    assert(m.t.t eq m);

    // check static type and write-through
    val t : mutable.Matrix[Int] = m.t;
    assert(t === DenseMatrix((1,4),(2,5),(3,6)));
    t(0,1) = 0;
    assert(m === DenseMatrix((1,2,3),(0,5,6)));
  }

  test("Min/Max") {
    val m = DenseMatrix((1,0,0),(2,3,-1));
    assert(m.argmin === (1,2));
    assert(m.argmax === (1,1));
    assert(m.min === -1);
    assert(m.max === 3);
  }

  test("MapValues") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,0),(2,3,-1));
    
    val b1 : DenseMatrix[Int] = a.mapValues(_ + 1);
    assert(b1 === DenseMatrix((2,1,1),(3,4,0)));
    
    val b2 : DenseMatrix[Double] = a.mapValues(_ + 1.0);
    assert(b2 === DenseMatrix((2.0,1.0,1.0),(3.0,4.0,0.0)));
  }

  test("Map") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,0),(2,3,-1));
    
    val b1 : DenseMatrix[Int] = a.map((i,j,v) => i + v);
    assert(b1 === DenseMatrix((1,0,0),(3,4,0)));
    
    val b2 : DenseMatrix[Double] = a.map((i,j,v) => j + v.toDouble);
    assert(b2 === DenseMatrix((1.0,1.0,2.0),(2.0,4.0,1.0)));
  }

  test("ForComprehensions") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,0),(2,3,-1));
    
    var s = 0;
    
    // foreach
    s = 0;
    for ((i,j,v) <- a) s += v;
    assert(s === a.sum);
    
    // filter
    s = 0;
    for ((i,j,v) <- a; if i % 2 == 0 || j % 2 == 0) s += v;
    assert(s === 1+2-1);
    
    // map
    val b1 : DenseMatrix[Double] = for ((i,j,v) <- a) yield v * 2.0;
    assert(b1 === DenseMatrix((2.0,0.0,0.0),(4.0,6.0,-2.0)));
    
    // map with filter
    val b2 : DenseMatrix[Int] = for ((i,j,v) <- a; if j == 0) yield v * 2;
    assert(b2 === DenseMatrix((2,0,0),(4,0,0)));
  }
    

  test("Tabulate") {
    val m : DenseMatrix[Double] = DenseMatrix.tabulate(2,2)((i,j) => 1.0 * (i+1) * (j+2));
    assert(m === DenseMatrix((2,3),(4,6)));
  }

  test("Multiply") {
    val a = DenseMatrix((1, 2, 3),(4, 5, 6));
    val b = DenseMatrix((7, -2, 8),(-3, -3, 1),(12, 0, 5));
    val c = DenseVector(6,2,3);
    assert(a * b === DenseMatrix((37, -8, 25), (85, -23, 67)));
    assert(a * c === DenseVector(19,52));
  }

  test("Trace") {
    assert(DenseMatrix((1,2,3),(4,5,6)).trace === 1 + 5);
    assert(DenseMatrix((1,2),(3,4),(5,6)).trace == 1 + 4);
    assert(DenseMatrix((1,2,3),(4,5,6),(7,8,9)).trace === 1 + 5 + 9);
  }

  test("Reshape") {
    val m : DenseMatrix[Int] = DenseMatrix((1,2,3),(4,5,6));
    val r : DenseMatrix[Int] = m.reshape(3,2);
    assert(m.data eq r.data);
    assert(r.numRows === 3);
    assert(r.numCols === 2);
    assert(r === DenseMatrix((1,5),(4,3),(2,6)));
  }

  test("Solve") {
    // square solve
    val r1 : DenseMatrix[Double] = DenseMatrix((1.0,3.0),(2.0,0.0)) \ DenseMatrix((1.0,2.0),(3.0,4.0));
    assert(r1 === DenseMatrix((1.5, 2.0), (-1.0/6, 0.0)));

    // matrix-vector solve
    val r2 : DenseVector[Double] = DenseMatrix((1.0,3.0,4.0),(2.0,0.0,6.0)) \ DenseVector(1.0,3.0);
    assert(r2 === DenseVector(0.1813186813186811, -0.3131868131868131, 0.43956043956043944));

    // wide matrix solve
    val r3 : DenseMatrix[Double] = DenseMatrix((1.0,3.0,4.0),(2.0,0.0,6.0)) \ DenseMatrix((1.0,2.0),(3.0,4.0));
    assert(r3 === DenseMatrix((0.1813186813186811,   0.2197802197802196),
                              (-0.3131868131868131, -0.1978021978021977),
                              (0.43956043956043944,  0.5934065934065933)));

    // tall matrix solve
    val r4 : DenseMatrix[Double] = DenseMatrix((1.0,3.0),(2.0,0.0),(4.0,6.0)) \ DenseMatrix((1.0,4.0),(2.0,5.0),(3.0,6.0));
    assert(r4 === DenseMatrix((0.9166666666666667,    1.9166666666666672),
                             (-0.08333333333333352, -0.08333333333333436)));
  }
}
