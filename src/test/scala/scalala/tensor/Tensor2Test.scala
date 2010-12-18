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

import domain._;

@RunWith(classOf[JUnitRunner])
class Tensor2Test extends FunSuite with Checkers {
  def mkTensor2() = {
    val domain = Product2Domain(SetDomain(Set("a","b","c")), SetDomain(Set(1,2)));
    mutable.Tensor2[String,Int,Double](domain);
  }

  def typeOf[X](value : X)(implicit m : scala.reflect.Manifest[X]) =
    m.toString;
  def show[X](value : X)(implicit m : scala.reflect.Manifest[X]) = {
    println(typeOf(value) + ":");
    println(value);
    println();
  }

  test("Getting and setting") {
    val x = mkTensor2();
    x("a",1) = 3.0;
    x("b",2) = 7.75;
    x("c",2) = 8.0;

    assert(x.valuesIterator.toList === List(3.0,0,0,7.75,0,8.0));
  }

  test("Transpose") {
    val x = mkTensor2();
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0);

    assert(x.t.valuesIterator.toList === List(3.0,0,0,0,7.75,8.0));
    assert(x.t.t === x);
    assert(x.t.t eq x);

    x.t(2,"a") = 1;
    assert(x("a",2) === 1);
  }

  test("Slice table") {
    val x= mkTensor2();
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0);

    val table = x(List("a","b","c"),List(1,2));

    assert(table.domain === TableDomain(3,2));

    table(1,0) = 5;
    assert(x("b",1) === 5);
  }

  test("Slice rows and columns") {
    val x= mkTensor2();
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0);

    assert(x("a", ::).isInstanceOf[Tensor1[_,_]]);
    assert(x("a", ::).toMap === Map(1->3.0, 2->0))

    assert(x(::, 2).isInstanceOf[Tensor1[_,_]]);
    assert(x(::, 2).toMap === Map("a" -> 0.0, "b" -> 7.75, "c" -> 8.0))
  }
}
