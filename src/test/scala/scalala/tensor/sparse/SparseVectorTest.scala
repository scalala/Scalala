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
class SparseVectorTest extends scalala.library.Library with scalala.library.Random with scalala.ScalalaTest {
  test("SparseVector:General") {
    val sparse = new SparseVector(10);
    val dense  = new scalala.tensor.dense.DenseVector(10);

    val values = List((2,2),(4,4),(3,3),(0,-1),(5,5),(1,1),(9,9),(7,7),(8,8),(3,3));

    for ((index,value) <- values) {
      sparse(index) = value;
      dense(index) = value;
      assertEquals(dense, sparse);
    }
  }

  test("SparseVector:Dot") {
    val x = new SparseVector(10);
    val y = new SparseVector(10);
    val d = rand(10);

    def densedot(a : Vector, b : Vector) =
      new DenseVector(a.toArray) dot new DenseVector (b.toArray);

    def checks() = {
      assertEquals(densedot(x,y), x dot y);
      assertEquals(densedot(y,x), y dot x);
      assertEquals(densedot(x,d), x dot d);
      assertEquals(densedot(d,x), d dot x);
      assertEquals(densedot(y,d), y dot d);
      assertEquals(densedot(d,y), d dot y);
    }

    x(2) = 3;
    y(2) = 4;
    checks();

    x(7) = 2;
    y += 1;
    checks();

    y -= 1;
    y(7) = .5;
    checks();

    x += 1;
    y(8) = 2;
    checks();

    y += 1;
    checks();
  }
}
