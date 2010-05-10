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

import tensor.Vector;

trait StatisticsTest extends Library with Statistics with scalala.ScalalaTest {
  test("CorrTest") {
    assertEquals(corr(Vector(1,2,3), Vector(2,3,3.4)), 0.97072, 1e-5);
    assertThrows[IllegalArgumentException](corr(Vector(1,2), Vector(2,3,3.4)));
    assertThrows[IllegalArgumentException](corr(Vector(), Vector()));
  }
}
