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
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith;

import scalala.tensor.mutable.{Matrix,Vector}
import scalala.library.LinearAlgebra._;

@RunWith(classOf[JUnitRunner])
class LinearAlgebraTest extends FunSuite with Checkers {
  test("kron") {
    val a = Matrix((1,2),(3,4));
    val b = Matrix((0,5),(6,7));
    assert(kron(a,b) === Matrix((0,5,0,10),(6,7,12,14),(0,15,0,20),(18,21,24,28)));
  }

  test("ranks") {
    assert(ranks(Array(1,2,3)).toList  === List(1.0,2.0,3.0));
    assert(ranks(Array(3,-1,2)).toList === List(3.0,1.0,2.0));
    assert(ranks(Array(1,2,3,3)).toList === List(1.0,2.0,3.5,3.5));
    assert(ranks(Array(1,2,3,3,3)).toList === List(1.0,2.0,4.0,4.0,4.0));
  }

  test("cholesky") {
    val A = Matrix((1.,0.,0.),(2.,3.,0.),(4.,5.,6.))
    val Sigma = A * A.t
    assert(cholesky(Sigma) === A)
  }

  test("eigSym") {
    val A = Matrix((9.,0.,0.),(0.,82.,0.),(0.,0.,25.))
    val (lambda, Some(evs)) = eigSym(A, true)
    assert(lambda === Vector(9.,25.,82.))
    assert(evs === Matrix((1.,0.,0.),(0.,0.,1.),(0.,1.,0.)))
  }
}

