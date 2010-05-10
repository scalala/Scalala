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
package library;

import org.junit.runner.RunWith;
import org.scalatest.junit.JUnitRunner;

/**
 * Unit tests for PartialMaps functions.
 *
 * @author dramage
 */
@RunWith(classOf[JUnitRunner])
class PartialMapsTest extends Library with PartialMaps with scalala.ScalalaTest {
  test("PartialMap:MinMax") {
    val v = new scalala.tensor.sparse.SparseVector(10);
    v(3) = 1;
    assertEquals(1, max(v));
    assertEquals(3, argmax(v));
    assertEquals(0, min(v));
    assertEquals(0, argmin(v));

    v += 2;
    v(3) = 1;
    assertEquals(2, max(v));
    assertEquals(0, argmax(v));
    assertEquals(1, min(v));
    assertEquals(3, argmin(v));
  }
}
