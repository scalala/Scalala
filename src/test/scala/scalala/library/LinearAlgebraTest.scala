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
package scalala;
package library;

import scalala.tensor.dense._;
import org.junit.runner.RunWith;
import org.scalacheck.Prop
import org.scalatest.junit.JUnitRunner;

/**
 * Some tests for the LinearAlgebra package
 *
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class LinearAlgebraTest extends Library with LinearAlgebra with Implicits with Random with scalala.ScalalaTest {

  import tensor.sparse.SparseVector;

  test("Basic determinants") {
    // this guy is singular
    val a = DenseMatrix(3,3)(1,2,3,4,5,6,7,8,9);
    assert(det(a) == 0);
  }

  test("-det(A) == det(-A)") {
     check(Prop.forAll { (m:DenseMatrix) =>
       val sq = m * m.t value;
       assertEquals(det(sq),-det(-sq),1E-4);
     });
  }


  test("det(a) == det(a.t)") {
     check(Prop.forAll { (m:DenseMatrix) =>
       val sq = m * m.t value;
       det(sq) == det(sq.t value);
     });
  }

  test("cholesky(a) * cholesky(a).t == a") {
     check(Prop.forAll { (m:DenseMatrix) =>
       val sq = m * m.t value;
       val l = cholesky(sq);
       assertEquals(sq, l * l.t value,1E-4);
     });
  }
}
