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

import org.scalacheck._
import org.scalatest._;
import org.scalatest.matchers.ShouldMatchers;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith;

import scalala.tensor.mutable.Vector;
import scalala.library.Statistics._;

@RunWith(classOf[JUnitRunner])
class StatisticsTest extends FunSuite with Checkers with ShouldMatchers {
  test("corr") {
    corr(Vector(1,2,3), Vector(2,3,3.4)) should be (0.97072 plusOrMinus 1e-5);
    assert(corr(Vector[Double](), Vector[Double]()).isNaN);
  }
  
  test("kendall") {
    val a = Vector(2.5,2.5,2.5,2.5,5,6.5,6.5,10,10,10,10,10,14,14,14,16,17);
    val b = Vector(1,1,1,1,2,1,1,2,1,1,1,1,1,1,2,2,2);
    kendall(a,b) should be (0.40754 plusOrMinus 1e-5);
    
    val x = Vector(1.5,1.5,3,4,6,6,6,8,9.5,9.5,11,12);
    val y = Vector(2.5,2.5,7,4.5,1,4.5,6,11.5,11.5,8.5,8.5,10);
    kendall(x,y) should be (0.55286 plusOrMinus 1e-5);
  }
}

