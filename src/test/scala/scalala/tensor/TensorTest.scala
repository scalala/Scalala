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

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TensorTest extends FunSuite with Checkers {
  def mkMapTensor() =
    mutable.Tensor[String,Int](domain.SetDomain(Set("a","b","c")));

  test("Get and set values") {
    val x = mkMapTensor();
    assert(x("a") === 0);
    assert(x("b") === 0);
    assert(x("c") === 0);
    x("a") = 5; x("b") = 7; x("c") = 2;
    assert(x("a") === 5);
    assert(x("b") === 7);
    assert(x("c") === 2);
  }

  test("Slicing to a sequence") {
    val x = mkMapTensor();
    x("a","b","c") := List(1,2,3);
    assert(x("a") === 1);
    assert(x("b") === 2);
    assert(x("c") === 3);

    assert(x("c","a","b","b","a").valuesIterator.toList === List(3,1,2,2,1));
  }

  test("Slicing to new keys") {
    val x = mkMapTensor();
    x("a","b","c") := List(1,2,3);
    val y = x(0->"a", 1->"b", 2->"c");
    assert(y(0) === x("a"));
    assert(y(1) === x("b"));
    assert(y(2) === x("c"));
  }

  test("Map values") {
    val x = mkMapTensor();
    x("a","b","c") := List(1,2,3);
    assert(x.mapValues(_ % 2 == 0).valuesIterator.toList === List(false,true,false));
  }

  test("Filter values") {
    val x = mkMapTensor();
    x("a","b","c") := List(1,2,3);
    x(x.find(_ >= 2)) := 0;
    assert(x.valuesIterator.toList === List(1,0,0));
  }

  test("Find") {
    val x = mkMapTensor();
    x("a","b","c") := List(1,2,3);
    assert(x.find(_ >= 2).toList === List("b","c"));
  }

  test("Overlapping slice assignments") {
    val x = mkMapTensor();
    x("a","b","c") := List(1,2,3);
    x("a","b") := x("b","c");
    assert(x.valuesIterator.toList === List(2,3,3));
  }

  test("Sorting") {
    val x = mkMapTensor();
    x("a","b","c") := List(5,7,3);
    
    assert(x.argsort.toList === List("c","a","b"));

    assert(x.sorted.valuesIterator.toList === List(3,5,7));
  }

  test("Min/Max") {
    val x = mkMapTensor();
    x("a","b","c") := List(5,7,3);
    assert(x.min === 3);
    assert(x.max === 7);
    assert(x.argmin === "c");
    assert(x.argmax === "b");
  }

//  test("Addition") {
//    assert(mutable.Tensor("a"->1,"b"->2) + mutable.Tensor("a"->3) === Tensor("a"->4,"b"->2));
//    assert(mutable.Tensor("a"->3) + mutable.Tensor("a"->1,"b"->2) === Tensor("a"->4,"b"->2));
//  }
//  
//  test("Subtraction") {
//    assert(mutable.Tensor("a"->1,"b"->2) - mutable.Tensor("a"->3) === Tensor("a" -> -2, "b" -> 2));
//    assert(mutable.Tensor("a"->3) - mutable.Tensor("a"->1,"b"->2) === Tensor("a" -> 2, "b" -> -2));
//  }
//  
//  test("Multiplication") {
//    assert(mutable.Tensor("a"->1,"b"->2) :* mutable.Tensor("a"->3) === Tensor("a"->3));
//    assert(mutable.Tensor("a"->3) :* mutable.Tensor("a"->1,"b"->2) === Tensor("a"->3));
//  }

}
