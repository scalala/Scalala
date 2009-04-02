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
package scalala.tensor.operators;

import scalala.ScalalaTest._;

import scalala.tensor._;
import scalala.tensor.dense._;
import OperatorSupport._;

import scalala.Scalala.Vector;

/**
 * Test code for Operators.
 * 
 * @author dramage
 */
trait OperatorTest {
  def _vector_test() = {
    val x = Vector(1,2,3);
    
    x += 2;
    assertEquals(x, Vector(3,4,5));
    
    val y = Vector(2,2,2);
    val z = Vector(.5,2,1);
    
    x -= y :* z;
    assertEquals(x, Vector(2,0,3));
    
    assertEquals(2*.5+2*2+2*1, y.t * z);
    assertEquals(2*.5+2*2+2*1, y * z.t);
    assertEquals(2*.5+2*2+2*1, z.t * y);
    assertEquals(2*.5+2*2+2*1, z * y.t);
  }
  
  def _matrix_test() = {
    val A = new DenseMatrix(Array(1,2,3,4,5,6),2,3);
    val b = Vector(1,.5,1);
    
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (A*b).value);
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (b.t * A.t).value);
  }
  
  def _implicits_test() = {
    import TensorImplicits._;
    
    val x = Array(1.0,2.0,3.0);
    x *= 2.0;
    assertEquals(x.toList, List(2.0,4.0,6.0));
    
    val y = Array(3,2,1);
    // y *= 2;      // correct compile error: doesn't promote to mutable
    x :*= y;
    assertEquals(x.toList, List(6.0, 8.0, 6.0));
  }
}

  
object OperatorTest extends OperatorTest with TestConsoleMain {
}
