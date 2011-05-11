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


import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

import domain._;

@RunWith(classOf[JUnitRunner])
class Counter2Test extends FunSuite with Checkers {
  def typeOf[X](value : X)(implicit m : scala.reflect.Manifest[X]) =
    m.toString;
  def show[X](value : X)(implicit m : scala.reflect.Manifest[X]) = {
    println(typeOf(value) + ":");
    println(value);
    println();
  }

  test("Getting and setting") {
    val x = mutable.Counter2[String,Int,Double]();
    x("a",1) = 3.0;
    x("b",2) = 7.75;
    x("c",2) = 8.0;

    assert(x.valuesIterator.toSet === Set(3.0,8.0,7.75));
  }

  test("Transpose") {
    val x = mutable.Counter2[String,Int,Double]();
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0);

    assert(x.t.valuesIterator.toSet === Set(3.0,0.0, 0.0, 0.0, 8.0,7.75));
    assert(x.t.t === x);
    assert(x.t.t eq x);

    x.t(2,"a") = 1;
    assert(x("a",2) === 1);
  }

  test("Slice table") {
    val x= mutable.Counter2[String,Int,Double]();
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0);

    val table = x(List("a","b","c"),List(1,2));

    assert(table.domain === TableDomain(3,2));

    table(1,0) = 5;
    assert(x("b",1) === 5);
  }

  test("Slice rows and columns") {
    val x = mutable.Counter2[String,Int,Double]();
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0);

    // require expected static type
    val s1 : mutable.Counter[Int,Double] = x("a",::);
    assert(s1.toMap === Map(1->3.0));

    // write-through
    s1(1) = 4;
    assert(x("a",1) === 4.0);

    // require expected static type
    val s2 : mutable.Counter[String,Double] = x(::,2);
    assert(s2.toMap === Map("a" -> 0.0, "b" -> 7.75, "c" -> 8.0));
    
    // write-through
    s2("a") = 1;
    assert(x("a",2) === 1.0);
  }

  test("Addition") {
    // require expected static type
    val v1 : mutable.Counter2[String,String,Int] =
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
  
  test("Shaped Multiplication") {
    assert(Counter2((0,'a',1),(1,'a',2),(1,'b',3)) * Counter2(('a',0,1),('b',0,2)) ===
      Counter2((0,0,1),(1,0,8)));
  }
}
