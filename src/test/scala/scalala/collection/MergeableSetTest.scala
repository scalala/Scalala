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
package scalala.collection;

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner;

/**
 * Tests of the MergeableSet architecture.
 * 
 * @author dramage
 */
@RunWith(classOf[JUnitRunner])
class MergeableSetTest extends scalala.ScalalaTest {
  test("MergeableSet") {
    assertEquals(IntSpanSet(-1,9), IntSpanSet(-1,3) ++ IntSpanSet(4,9));
    assertEquals(IntSpanSet(4,7), IntSpanSet(2,7) ** IntSpanSet(4,14));
    assertEquals(IntersectionSet(IntSpanSet(2,5),IntSpanSet(6,14)),
                 IntSpanSet(2,5) ** IntSpanSet(6,14));

    // check that we don't repeat iterator in UnionSet's iterator
    assertEquals(List(0,1,2,3), UnionSet(IntSpanSet(0,3),IntSpanSet(2,4)).iterator.toList);
  }
}
