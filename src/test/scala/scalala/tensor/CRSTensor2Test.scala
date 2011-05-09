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
package scalala.tensor;


import dense.{DenseVectorCol, DenseVector}
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

import domain._;

@RunWith(classOf[JUnitRunner])
class CRSTensor2Test extends FunSuite with Checkers {
  def typeOf[X](value : X)(implicit m : scala.reflect.Manifest[X]) =
    m.toString;
  def show[X](value : X)(implicit m : scala.reflect.Manifest[X]) = {
    println(typeOf(value) + ":");
    println(value);
    println();
  }

  test("Getting and setting") {
    val x = mutable.CRSTensor2[String,Int,Double,DenseVectorCol[Double]](DenseVector.zeros(3));
    x("a",1) = 3.0;
    x("b",2) = 7.75;
    x("c",2) = 8.0;

    assert(x.valuesIterator.toSet === Set(3.0,8.0,7.75,0.0));
  }

  test("Slice rows") {
    val x = mutable.CRSTensor2[String,Int,Double,DenseVectorCol[Double]](DenseVector.zeros(3));
    x("a",::) := DenseVector(1.,2.,3.)

    // require expected static type
    val s1 : DenseVectorCol[Double] = x("a",::);
    assert(s1 === DenseVector(1.,2.,3.));

    // write-through
    s1(1) = 4;
    assert(x("a",1) === 4.0);

  }

  /*
  test("Addition") {
    // require expected static type
    val x = mutable.CRSTensor2[String,Int,Double,DenseVectorCol[Double]](DenseVector.zeros(3));
    x("a") := DenseVector(1.,2.,3.);
      Counter2(("a","a",1),("b","b",2)) + Counter2(("a","a",3));
    assert(v1 === Counter2(("a","a",4),("b","b",2)));
    
    // require expected static type
    val v2 : mutable.Counter2[String,Char,Int] =
      Counter2(("a",'a',3)) + Counter2(("a",'a',1),("b",'b',2));
    assert(v2 === Counter2(("a",'a',4),("b",'b',2)));
  }
  
  test("AddInto") {
    val x = Counter2[String,String,Int]();
    x += Counter2(("a","a",1));
    assert(x === Counter2(("a","a",1)));
    x += Counter2(("a","a",2),("a","b",4));
    assert(x === Counter2(("a","a",3),("a","b",4)));
  }

  test("Subtraction") {
    assert(mutable.Counter2(("a","a",1),("b","b",2)) - mutable.Counter2(("a","a",3)) === Counter2(("a","a",-2), ("b","b",2)));
    assert(mutable.Counter2(("a","a",3)) - mutable.Counter2(("a","a",1),("b","b",2)) === Counter2(("a","a",2), ("b","b",-2)));
  }

  test("Multiplication") {
    assert(mutable.Counter2(("a","a",1),("b","b",2)) :* mutable.Counter2(("a","a",3)) === Counter2(("a","a",3)));
    assert(mutable.Counter2(("a","a",3)) :* mutable.Counter2(("a","a",1),("b","b",2)) === Counter2(("a","a",3)));
  }
  */
}
