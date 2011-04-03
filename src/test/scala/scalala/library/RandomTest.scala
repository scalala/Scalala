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
package scalala.library

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import scalala.library.Random._
import scalala.library.Statistics._
import scalala.tensor.dense._

@RunWith(classOf[JUnitRunner])
class RandomTest extends FunSuite with Checkers {

  test("multivariateGaussian") {
    implicit val mt = new random.MersenneTwisterFast(0l);
    
    val Sigma = DenseMatrix((3., 4.), (4., 16.))
    val mu    = DenseVector(77.,-3.)
    val X     = randn(mu, Sigma, 500000)

    val (chkSigma, chkMu) = covariance(X)
    assert(chkMu forall ( (i,v) => math.abs(v-mu(i)) < 1e-1 ))
    assert(chkSigma forall ( (idx,v) => math.abs(v-Sigma(idx)) < 1e-1 ))
  }
}

