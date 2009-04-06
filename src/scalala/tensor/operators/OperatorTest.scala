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
  
  def _matrix_vector_test() = {
    val A = new DenseMatrix(Array(1,2,3,4,5,6),2,3);
    val b = Vector(1,.5,1);
    
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (A*b).value);
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (b.t * A.t).value);
  }
  
  def _matrix_matrix_test() = {
    val A = new DenseMatrix(Array(0.652639,0.954604,0.112813,0.594183,0.086937,0.689550,0.941195,0.429511,0.970387,0.357748,0.984985,0.416206), 3, 4);
    val B = new DenseMatrix(Array(0.595799,0.115880,0.941191,0.786414,0.019879,0.533605,0.537067,0.612046),4,2);
    val C = new DenseMatrix(Array(1.62488,1.75768,1.38775,1.05448,0.89890,1.14609),3,2);
    assertEquals((A * B).value.join(C)((v1:Double,v2:Double) => Math.abs(v1 - v2) < 1e-5), C.map((x:Double) => true));
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
