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
package scalala.library

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner;

/**
 * Some tests for the vectors package.
 *
 * @author dramage
 */

@RunWith(classOf[JUnitRunner])
class VectorsTest extends Library with Vectors with Implicits with Random with scalala.ScalalaTest {

  import scalala.tensor.sparse.SparseVector;

  test("Tensor:Moments") {
    val v = new SparseVector(1000);
    v += 1;
    v(0 until 100) = rand(100).valuesIterator.toSeq;
    assertEquals(mean(v.toArray:Iterable[Double]), mean(v), 1e-10);
    assertEquals(variance(v.toArray:Iterable[Double]), variance(v), 1e-10);
    assertEquals(std(v.toArray:Iterable[Double]), std(v), 1e-10);

    assertEquals((1 + 3 + 22 + 17) / 4.0, mean(Vector(1,3,22,17)), 1e-10);

    assertEquals(0.08749136216928063,
                 variance(Vector(0.29854716128994807,0.9984567314422015,0.3056949899038196,
                                 0.8748240977963917,0.6866542395503176,0.48871321020847913,
                                 0.23221169231853678,0.992966911646403,0.8839015907147733,
                                 0.6435495508602755)),
                 1e-10);
  }

  test("Tensor:Norm") {
    val v = Vector(-0.4326,-1.6656,0.1253,0.2877,-1.1465);
    assertEquals(norm(v,1), 3.6577, 1e-4);
    assertEquals(norm(v,2), 2.0915, 1e-4);
    assertEquals(norm(v,3), 1.8405, 1e-4);
    assertEquals(norm(v,4), 1.7541, 1e-4);
    assertEquals(norm(v,5), 1.7146, 1e-4);
    assertEquals(norm(v,6), 1.6940, 1e-4);
    assertEquals(norm(v,Double.PositiveInfinity), 1.6656, 1e-4);
  }
}
