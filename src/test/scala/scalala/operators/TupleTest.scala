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
package operators;

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

import Implicits._;

@RunWith(classOf[JUnitRunner])
class TupleTest extends FunSuite with Checkers {

  test("Neg") {
    assert(-(1,2.0,3f) === (-1, -2.0, -3f));
  }

  test("Add") {
    assert((1,2,3l) + (3.0,2,-1) === (4.0,4,2l));
    assert((1,2,3,4) + 1 === (2,3,4,5));
    assert(2f :+ (1,2.0,3,4,6) === (3f,4.0,5f,6f,8f));
  }

  test("Sub") {
    assert((1,2,3l) - (3.0,2,-1) === (-2.0,0,4l));
    assert((1,2,3,4) - 1 === (0,1,2,3));
    assert(7.0 :- (1,2,3,4) === (6.0,5.0,4.0,3.0));
  }

  test("Mul") {
    assert((1,2) :* (2,4) === (2,8));
    assert((1,2,3,4) :* 2.0 === (2.0,4.0,6.0,8.0));
    assert(0.5f :* (1,2,3,4) === (0.5f,1.0f,1.5f,2.0f));
  }
  
  test("Div") {
    assert((4,2) :/ (2,2) === (2,1));
    assert((1,2,3,4) :/ 2.0 === (0.5,1.0,1.5,2.0));
    assert(2f :/ (1,2,4) === (2f,1f,0.5f));
  }
  
  test("Mod") {
    assert((3,5) :% 2 === (1,1));
  }
  
  test("Compare") {
    assert(((1,2,3) :<  (3,2,1)) === (true,false,false));
    assert(((1,2,3) :<= (3,2,1)) === (true,true,false));
    assert(((1,2,3) :>  (3,2,1)) === (false,false,true));
    assert(((1,2,3) :>= (3,2,1)) === (false,true,true));
    assert(((1,2,3) :== (3,2,1)) === (false,true,false));
    assert(((1,2,3) :!= (3,2,1)) === (true,false,true));
  }
}

