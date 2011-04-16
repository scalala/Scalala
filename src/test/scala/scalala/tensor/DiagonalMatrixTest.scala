package scalala.tensor

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

import dense.{DenseVector, DenseMatrix}
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith
import scalala.library.LinearAlgebra.diag;

@RunWith(classOf[JUnitRunner])
class DiagonalMatrixTest extends FunSuite with Checkers {


  test("Multiply") {
    val a = DenseMatrix((1, 2, 3),(4, 5, 6),(7,8,9));
    val c = diag(DenseVector(6,2,3));
    val ac :DenseMatrix[Int] = a * c; // check types;
    assert(a * c === DenseMatrix( (6,4,9),(24,10,18),(42,16,27)));
  }




}
