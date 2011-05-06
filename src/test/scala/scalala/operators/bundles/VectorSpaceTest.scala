package scalala
package operators
package bundles
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

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith
import tensor.dense.{DenseVectorCol, DenseVector}

// This test basically just makes sure things compile
@RunWith(classOf[JUnitRunner])
class VectorSpaceTest extends FunSuite with Checkers {
  type DVectorSpace[V] = MutableVectorSpace[Double,V];
  def vsAdd[V:DVectorSpace](v1: V, v2: V) = {
    val dv = implicitly[DVectorSpace[V]];
    import dv._;
    v1 + v2;
  }
  test("VectorSpace basically works") {
    VectorSpace.make[DenseVectorCol[Double],Double];
    val r = vsAdd(DenseVector(1.,2.,3.),DenseVector(3.,4.,5.))
    assert(r === DenseVector(4.,6.,8.));
  }
}

