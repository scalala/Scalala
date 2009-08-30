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

import scalala.tensor._;
import scalala.tensor.dense._;

import scalala.Scalala.Vector;

/**
 * Test code for Operators.
 * 
 * @author dramage
 */
trait OperatorTest extends scalala.library.Library with OperatorImplicits with scalala.ScalalaTest {
  private def typeOf[A](x : A)(implicit m : scala.reflect.Manifest[A]) =
    m.toString;
  
//  test("Operators:InlineOptimizations") {
//    val x = Vector(1,2,3);
//    assertEquals(x + 3, (x + 1) + 2);
//    assertEquals(x + 3, (x + 4) - 1);
//    assertEquals(x * 6, (x * 2) * 3);
//    assertEquals(x * 3, (x * 6) / 2);
//    assertEquals(3 / x, (6 / x) / 2);
//    assertEquals(6 / x, (3 / x) * 2);
//  }
                           
  /** Tests basic tensor-scalar operators. */
  test("Operators:Tensor-Scalar") {
    def build(values : Double*) : Tensor[Int] =
      Vector(values :_*);
    
    def x = build(0,1,2,3);
    
    assertEquals(-x      value, build(0,-1,-2,-3));
    assertEquals(x   + 1 value, build(1,2,3,4));
    assertEquals(x   - 1 value, build(-1,0,1,2));
    assertEquals(x   * 2 value, build(0,2,4,6));
    assertEquals(x   / 2 value, build(0,.5,1,1.5));
    assertEquals(x  :^ 2 value, build(0,1,4,9));
    assertEquals(x   < 2 value, build(1,1,0,0));
    assertEquals(x   > 2 value, build(0,0,0,1));
    assertEquals(x  <= 2 value, build(1,1,1,0));
    assertEquals(x  >= 2 value, build(0,0,1,1));
    assertEquals(x  && 1 value, build(0,1,1,1));
    assertEquals(x  && 1 value, build(0,1,1,1));
    assertEquals(x  || 1 value, build(1,1,1,1));
    assertEquals(x :== 2 value, build(0,0,1,0));
    assertEquals(x :!= 2 value, build(1,1,0,1));
  }
  
  /** Tests basic tensor-scalar operators. */
  test("Operators:Vector-Scalar") {
    def build(values : Double*) : Vector =
      Vector(values :_*);
    
    def x = build(0,1,2,3);
    
    assertEquals(-x      value, build(0,-1,-2,-3));
    assertEquals(x   + 1 value, build(1,2,3,4));
    assertEquals(x   - 1 value, build(-1,0,1,2));
    assertEquals(x   * 2 value, build(0,2,4,6));
    assertEquals(x   / 2 value, build(0,.5,1,1.5));
    assertEquals(x  :^ 2 value, build(0,1,4,9));
    assertEquals(x   < 2 value, build(1,1,0,0));
    assertEquals(x   > 2 value, build(0,0,0,1));
    assertEquals(x  <= 2 value, build(1,1,1,0));
    assertEquals(x  >= 2 value, build(0,0,1,1));
    assertEquals(x  && 1 value, build(0,1,1,1));
    assertEquals(x  && 1 value, build(0,1,1,1));
    assertEquals(x  || 1 value, build(1,1,1,1));
    assertEquals(x :== 2 value, build(0,0,1,0));
    assertEquals(x :!= 2 value, build(1,1,0,1));
  }
  
  /** Tests Tensor-Tensor operations. */
  test("Operators:Tensor-Tensor") {
    def build(values : Double*) : Tensor[Int] =
      Vector(values :_*);
    
    def x = build(0,1,2,3);
    def y = build(2,1,2,-1);
    assertEquals(x   + y value, build(2,2,4,2));
    assertEquals(x   - y value, build(-2,0,0,4));
    assertEquals(x  :* y value, build(0,1,4,-3));
    assertEquals(x  :/ y value, build(0,1,1,-3));
    assertEquals(x  :^ y value, build(0,1,4,1.0/3.0));
    assertEquals(x  :< y value, build(1,0,0,0));
    assertEquals(x  :> y value, build(0,0,0,1));
    assertEquals(x :<= y value, build(1,1,1,0));
    assertEquals(x :>= y value, build(0,1,1,1));
    assertEquals(x :== y value, build(0,1,1,0));
    assertEquals(x :!= y value, build(1,0,0,1));
    assertEquals(x :&& y value, build(0,1,1,1));
    assertEquals(x :|| y value, build(1,1,1,1));
  }
  
  /** Tests Vector-Vector operations. */
  test("Operators:Vector-Vector") {
    def build(values : Double*) : Vector =
      Vector(values :_*);
    
    def x = build(0,1,2,3);
    def y = build(2,1,2,-1);
    
    assertEquals(x   + y value, build(2,2,4,2));
    assertEquals(x   - y value, build(-2,0,0,4));
    assertEquals(x  :* y value, build(0,1,4,-3));
    assertEquals(x  :/ y value, build(0,1,1,-3));
    assertEquals(x  :^ y value, build(0,1,4,1.0/3.0));
    assertEquals(x  :< y value, build(1,0,0,0));
    assertEquals(x  :> y value, build(0,0,0,1));
    assertEquals(x :<= y value, build(1,1,1,0));
    assertEquals(x :>= y value, build(0,1,1,1));
    assertEquals(x :== y value, build(0,1,1,0));
    assertEquals(x :!= y value, build(1,0,0,1));
    assertEquals(x :&& y value, build(0,1,1,1));
    assertEquals(x :|| y value, build(1,1,1,1));
  }

//  test("Operators:Tensor1-Tensor2") {
//    def build1(values : Double*) : Tensor1[Int] =
//      Vector(values :_*);
//    def build2(rows : Int, cols : Int)(values : Double*) : Tensor2[Int,Int] =
//      DenseMatrix(rows,cols)(values :_*);
//    
//    def x = build1(0,1,2,3);
//    def y = build1(2,1,2,-1);
//    def A = build2(4,4)(7,3,6,-2,4,2,6,8,1,0,0,1,3,-1,-1,-1);
//    def B = build2(4,4)(3,2,1,2,7,8,-1,9,-2,-3,-2,1,5,0,0,0);
//    
//    // many other permutations (orderings, transposes) should
//    // result in compile time errors
//    
//    assertEquals(x.t * y, 0*2 + 1*1 + 2*2 - 3*1);
//    assertEquals(x * y.t value, build2(4,4)(0,2,4,6,0,1,2,3,0,2,4,6,0,-1,-2,-3));
//    assertEquals(A * x value, build1(15,-1,3,7));
//    assertEquals(A * y value, build1(17,9,19,7));
//    assertEquals(x.t * A   value, build1(9,38,3,-6));
//    assertEquals(x.t * A.t value, build1(15,-1,3,7));
//    assertEquals(y.t * A   value, build1(31,14,1,4));
//    assertEquals(y.t * A.t value, build1(17,9,19,7));
//    assertEquals(A * B   value, build2(4,4)(36,11,28,9,107,28,81,40,-25,-13,-31,-23,35,15,30,-10));
//    assertEquals(A * B.t value, build2(4,4)(62,18,55,43,43,22,60,57,1,1,0,-12,51,24,66,69));
//  }
  
  /**
   * Same as Tensor1-Tensor2 except using alternate Vector-Matrix
   * implicit conversion control path.
   */
  test("Operators:Vector-Matrix") {
    def build1(values : Double*) : Vector =
      Vector(values :_*);
    def build2(rows : Int, cols : Int)(values : Double*) : Matrix =
      DenseMatrix(rows,cols)(values :_*);
    
    def x = build1(0,1,2,3);
    def y = build1(2,1,2,-1);
    def A = build2(4,4)(7,3,6,-2,4,2,6,8,1,0,0,1,3,-1,-1,-1);
    def B = build2(4,4)(3,2,1,2,7,8,-1,9,-2,-3,-2,1,5,0,0,0);
    
    // many other permutations (orderings, transposes) should
    // result in compile time errors
    
    assertEquals(x.t * y, 0*2 + 1*1 + 2*2 - 3*1);
    assertEquals(x * y.t value, build2(4,4)(0,2,4,6,0,1,2,3,0,2,4,6,0,-1,-2,-3));
    assertEquals(A * x value, build1(15,-1,3,7));
    assertEquals(A * y value, build1(17,9,19,7));
    assertEquals(x.t * A   value, build1(9,38,3,-6));
    assertEquals(x.t * A.t value, build1(15,-1,3,7));
    assertEquals(y.t * A   value, build1(31,14,1,4));
    assertEquals(y.t * A.t value, build1(17,9,19,7));
    assertEquals(A * B   value, build2(4,4)(36,11,28,9,107,28,81,40,-25,-13,-31,-23,35,15,30,-10));
    assertEquals(A * B.t value, build2(4,4)(62,18,55,43,43,22,60,57,1,1,0,-12,51,24,66,69));
  }
  
  
  /** Tests extra operators on dense matrices. */
  test("Operators:Dense") {
    val _A = new DenseMatrix(2, 2, Array(1.0, 2.0, 3.0, 4.0));
    val _B = new DenseMatrix(2, 2, Array(2.0, 0.0, 3.0, 3.0));
    assertEquals(new DenseMatrix(2, 2, Array(-4.0, 2.0, -1.5, 1.5)), (_A \ _B) value);
    
    val _C = new DenseMatrix(2, 3, Array(1.0, 2.0, 3.0, 4.0, 5.0, -1.0));
    val _D = new DenseMatrix(2, 2, Array(2.0, 0.0, 3.0, 3.0));
    assertEquals(new DenseMatrix(3, 2, Array(0.0091743, 0.0825688, 0.3486239, 0.2935780, 0.6422018, 0.1559633)), (_C \ _D).value, 1e-7);
    
    assertEquals(Vector(0.0091743, 0.0825688, 0.3486239), (_C \ Vector(2, 0)).value, 1e-7);
  }
  
  /** Tests that static types are correctly propogated through operators. */
  test("Operators:StaticTypePropogation") {
    assertEquals(
      typeOf(Vector(1,2,3):Tensor[Int]),
      typeOf( (Vector(1,2,3):Tensor[Int]) + 1 value));
    
    assertEquals(
      typeOf(Vector(1,2,3):Tensor1[Int]),
      typeOf( (Vector(1,2,3):Tensor1[Int]) + 1 value));
    
    assertEquals(
      typeOf(Vector(1,2,3):Vector),
      typeOf( (Vector(1,2,3):Vector) + 1 value));
    
    assertEquals(
      typeOf(Vector(1,2,3)),
      typeOf(Vector(1,2,3) + 1 value));
  }
  
  test("Operators:Vector") {
    val x = Vector(1,2,3);
    
    assertEquals(new DenseMatrix(3, 3, Array(1.,2.,3.,2.,4.,6.,3.,6.,9.)), (x * x.t).value);
    
    x += 2;
    assertEquals(x, Vector(3,4,5));
    
    val y = Vector(2,2,2);
    val z = Vector(.5,2,1);
    
    x -= y :* z;
    assertEquals(x, Vector(2,0,3));
    
    assertEquals(2*.5+2*2+2*1, y.t * z);
    assertEquals(2*.5+2*2+2*1, z.t * y);
  }
  
  test("Operators:Matrix") {
    val A = new DenseMatrix(2, 3, Array(1,2,3,4,5,6));
    val b = Vector(1,.5,1);
    
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (A*b).value);
    assertEquals(Vector(1*1+.5*3+1*5, 1*2+.5*4+1*6), (b.t * A.t).value);
  }
  
  test("Operators:Matrix-Matrix") {
    val A = new DenseMatrix(3, 4, Array(0.652639,0.954604,0.112813,0.594183,0.086937,0.689550,0.941195,0.429511,0.970387,0.357748,0.984985,0.416206));
    val B = new DenseMatrix(4, 2, Array(0.595799,0.115880,0.941191,0.786414,0.019879,0.533605,0.537067,0.612046));
    val C = new DenseMatrix(3, 2, Array(1.62488,1.75768,1.38775,1.05448,0.89890,1.14609));
    assertEquals(A * B value, C value, 1e-5);
  }
  
  test("Operators:ArrayMath") {
    import TensorImplicits._;
    
    val x = Array(1.,2.,3.);
    x *= 2.0;
    assertEquals(x.toList, List(2.0,4.0,6.0));
    
    val y = Array(3.,2.,1.);

    assertEquals(x :* y value, Vector(6,8,6));
    
    // y *= 2;      // correct compile error: Array[Int] doesn't promote to mutable
    x :*= Array(3,2,1);
    assertEquals(x.toList, List(6.0, 8.0, 6.0));
  }
  
  test("Operators:Scalars") {
    val x = Vector(1,1,1);
    x += Vector(1,2,3) * 2.0;
    assertEquals(x, Vector(3,5,7));
    
//    x := 2 / Vector(1,2,3);
//    assertEquals(x, Vector(2,1,2./3.));
  }
}

object OperatorTest extends OperatorTest with scalala.ScalalaTest.TestConsoleMain;
