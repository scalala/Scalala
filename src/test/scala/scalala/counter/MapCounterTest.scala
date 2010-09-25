///*
// * Distributed as part of Scalala, a linear algebra library.
// *
// * Copyright (C) 2008- Daniel Ramage
// *
// * This library is free software; you can redistribute it and/or
// * modify it under the terms of the GNU Lesser General Public
// * License as published by the Free Software Foundation; either
// * version 2.1 of the License, or (at your option) any later version.
// *
// * This library is distributed in the hope that it will be useful,
// * but WITHOUT ANY WARRANTY; without even the implied warranty of
// * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// * Lesser General Public License for more details.
// *
// * You should have received a copy of the GNU Lesser General Public
// * License along with this library; if not, write to the Free Software
// * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
// */
//package scalala;
//package counter;
//
//import org.scalacheck._
//import org.scalatest._;
//import org.scalatest.junit._;
//import org.scalatest.prop._;
//import org.junit.runner.RunWith
//
//@RunWith(classOf[JUnitRunner])
//class HashMapCounterTest extends FunSuite with Checkers {
//
//  test("Basics") {
//    val counter = HashMapCounter[Int,Double]();
//    assert(counter.size === 0);
//    counter(3) = 1.0;
//    counter(4) = 2.0;
//    assert(counter.size === 2);
//    assert(counter(3) === 1.0);
//    assert(counter(4) === 2.0);
//  }
//
//  test("Updates") {
//    val counter = HashMapCounter[Int,Double]();
//    assert(counter.domain.size === 0);
//    counter(3) = 1.0;
//    counter(4) = 2.0;
//    counter(1 to 5) += 3;
//    assert(counter(1,2,3,4,5).toList === List(3,3,4,5,3));
//  }
//
//  test("Min/Max") {
//    val counter = HashMapCounter[Int,Double]();
//    counter(3) = 1.0;
//    counter(4) = 2.0;
//    assert(counter.min === 1.0);
//    assert(counter.max === 2.0);
//    assert(counter.argmin === 3);
//    assert(counter.argmax === 4);
//  }
//}
