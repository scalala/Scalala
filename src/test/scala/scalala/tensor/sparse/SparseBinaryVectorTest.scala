/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala.tensor.sparse

import scalala.tensor.Vector;
import scalala.tensor.dense.DenseVector;

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner;

@RunWith(classOf[JUnitRunner])
class SparseBinaryVectorTest extends scalala.library.Library with scalala.library.Random with scalala.ScalalaTest {
  test("SparseBinaryVector") {
    val x = new SparseBinaryVector(10);
    val y = new SparseBinaryVector(10);
    val d = rand(10);
    val e = new SparseVector(10);
    e(2) = 3; e(4) = d(4); e(7) = d(7); e(9)=d(9);

    def densedot(a : Vector, b : Vector) =
      new DenseVector(a.toArray) dot new DenseVector(b.toArray);

    def checks() = {
      assertEquals(densedot(x,y), x dot y);
      assertEquals(densedot(y,x), y dot x);
      assertEquals(densedot(x,d), x dot d);
      assertEquals(densedot(x,e), x dot e);
      assertEquals(densedot(d,x), d dot x);
      assertEquals(densedot(e,x), e dot x);
      assertEquals(densedot(y,d), y dot d);
      assertEquals(densedot(y,e), y dot e);
      assertEquals(densedot(d,y), d dot y);
      assertEquals(densedot(e,y), e dot y);
    }

    checks();

    x(2) = 1;
    y(2) = 1;
    checks();

    x(7) = 1;
    checks();

    y(7) = 1;
    checks();

    y(2) = 0;
    checks();

    x(2) = 0;
    checks();
  }
}
