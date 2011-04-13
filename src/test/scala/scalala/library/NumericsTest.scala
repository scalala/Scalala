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
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import Library._
import Numerics._


@RunWith(classOf[JUnitRunner])
class NumericsTest extends FunSuite with Checkers with ShouldMatchers {

  test("logSum") {
    logSum(log(5), log(2)) should be (log(7) plusOrMinus 1e-10)
    logSum(log(2), log(5)) should be (log(7) plusOrMinus 1e-10)
    logSum(Double.NegativeInfinity, log(5)) should be (log(5) plusOrMinus 1e-10)
    logSum(log(5), Double.NegativeInfinity) should be (log(5) plusOrMinus 1e-10)
    logSum(Double.NegativeInfinity, Double.NegativeInfinity) should be (Double.NegativeInfinity)

    logSum(log(1), log(2), log(3)) should be (log(6) plusOrMinus 1e-10)
    logSum(log(1), log(2), Double.NegativeInfinity) should be (log(3) plusOrMinus(1e-10))

    val s = Array.tabulate[Double](5)(i => log1p(i))
    logSum(s.iterator, s.max) should be (log(15) plusOrMinus 1e-10)
    logSum(s) should be (log(15) plusOrMinus 1e-10)
    logSum(Double.NegativeInfinity +: s) should be (log(15) plusOrMinus 1e-10)
    logSum(s :+ Double.NegativeInfinity) should be (log(15) plusOrMinus 1e-10)
  }

  test("logDiff") {
    logDiff(log(5), log(2)) should be (log(3) plusOrMinus 1e-10)
    logDiff(log(5), log(5)) should be (Double.NegativeInfinity)

    evaluating {
      logDiff(log(5), log(6))
    } should produce [IllegalArgumentException]
  }

}
