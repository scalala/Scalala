/*
 * Distributed as part of Scalala, a linear algebra
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala
package library

import org.scalatest._
import matchers.ShouldMatchers
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import scalala.tensor.mutable.{Matrix, Vector}
import Library._
import tensor.dense.{DenseVectorCol, DenseVector}

@RunWith(classOf[JUnitRunner])
class LibraryTest extends FunSuite with Checkers with ShouldMatchers {

  test("log") {
    assert(log(2.8) === 1.0296194171811581)
    assert(log(1) === 0)
    assert(log(Array(1,2,3,4)).toList === List(0.0, 0.6931471805599453, 1.0986122886681098, 1.3862943611198906))
  }

  test("mean") {
    assert(mean(List(Array(0,2),Array(2,4))).toList === List(1,3))
    assert(mean(List(Array(1.0,3.0),Array(2.0,4.0))).toList === List(1.5,3.5))
    assert(mean(Iterator(1,2,3)) === 2.0)
    assert(mean(List(1,2,3,4)) === ((1+2+3+4) / 4.0))
  }

  test("stddev") {
    assert(stddev(Array(1,2,3)) === 1.0)
    assert(stddev(Iterator(1,2,3,4)) === 1.2909944487358056)
  }

  test("exp") {
    assert(exp(Array(1,2,3,4)).toList === List(1,2,3,4).map(_.toDouble).map(math.exp))
  }

  test("Tensor:Norm") {
    val v = Array(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    norm(v, 1) should be (3.6577 plusOrMinus 1e-4)
    norm(v, 2) should be (2.0915 plusOrMinus 1e-4)
    norm(v, 3) should be (1.8405 plusOrMinus 1e-4)
    norm(v, 4) should be (1.7541 plusOrMinus 1e-4)
    norm(v, 5) should be (1.7146 plusOrMinus 1e-4)
    norm(v, 6) should be (1.6940 plusOrMinus 1e-4)
    norm(v, Double.PositiveInfinity) should be (1.6656 plusOrMinus 1e-4)
  }

  test("Tensor:Normalize") {
    val v = DenseVector(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    val norm2V = normalize(v, 2)
    val norm1V = normalize(v, 1)
    norm(norm2V, 2) should be (1.0 plusOrMinus 1e-4)
    norm(norm1V, 1) should be (1.0 plusOrMinus 1e-4)
  }

  test("Tensor:LogNormalize") {
    val v = DenseVector(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    val normed = logNormalize(v)
    softmax(normed) should be (0.0 plusOrMinus 1e-4)
  }

  test("Matrix:Mean") {
    val X = Matrix((9,5,14,4),(2,-12,8,-5),(8,-6,-8,11))
    val meanV = Vector(6.3333, -4.3333, 4.6666, 3.3333)
    val meanH = Vector(8., -1.75, 1.25)
    assert(mean(X, Axis.Horizontal) forallPairs ((i, v) => math.abs(v-meanH(i)) < 1e-4))
    assert(mean(X, Axis.Vertical) forallPairs ((i, v) => math.abs(v-meanV(i)) < 1e-4))
  }

  test("Matrix:Covariance") {
    val X = Matrix((9.,5.,14.,4.),(2.,-12.,8.,-5.),(8.,-6.,-8.,11.))
    val mu = Vector(8., -1.75, 1.25)
    val Sigma = Matrix(( 20.6666, 35.3333, -22.0000),
                       ( 35.3333, 74.9166,  -7.4166),
                       (-22.0000, -7.4166,  92.9166))

    val (chkSigmaH, chkMuH) = covariance(X, Axis.Horizontal)
    assert(chkMuH forallPairs ((i, v) => math.abs(v-mu(i)) < 1e-4))
    assert(chkSigmaH forallPairs ((idx, v) => math.abs(v-Sigma(idx)) < 1e-4))

    val (chkSigmaV, chkMuV) = covariance(X.t, Axis.Vertical)
    assert(chkMuV forallPairs ((i, v) => math.abs(v-mu(i)) < 1e-4))
    assert(chkSigmaV forallPairs ((idx, v) => math.abs(v-Sigma(idx)) < 1e-4))
  }

  test("Matrix:MinMax") {
    val X = Matrix((9.,5.,14.,4.),(2.,-12.,8.,-5.),(8.,-6.,-8.,11.))
    min(X, Axis.Horizontal).toArray should be === (Array(4.,-12.,-8.))
    max(X, Axis.Horizontal).toArray should be === (Array(14.,8.,11.))
    min(X, Axis.Vertical).toArray should be === (Array(2.,-12.,-8.,-5.))
    max(X, Axis.Vertical).toArray should be === (Array(9.,5.,14.,11.))
  }

}

