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

package scalala.tensor.dense

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit._
import org.junit.runner.RunWith
import scalala.tensor.domain.IndexDomain
import scalala.library.random.MersenneTwisterFast

@RunWith(classOf[JUnitRunner])
class DenseVectorConstructorTest extends FunSuite with ShouldMatchers {

  test("DenseVectorRow") {
    val vd: DenseVectorRow[Double] = DenseVectorRow(1.0, 2.0, 3.0)
    vd.length should equal (3)
    (vd.values zip (1 to 3)) foreach { case (a,b) => a should equal (b) }

    val vf: DenseVectorRow[Float] = DenseVectorRow(Array(1.0f, 2.0f, 3.0f))
    vf.length should equal (3)
    (vf.values zip (1 to 3)) foreach { case (a,b) => a should equal (b) }

    val vdomi: DenseVectorRow[Int] = DenseVectorRow[Int](IndexDomain(10))
    vdomi.length should equal (10)
    vdomi.values foreach (_ should equal (0))

    val vdf: DenseVectorRow[Double] = DenseVectorRow.fill(23)(3.0)
    vdf.length should equal (23)
    vdf.values foreach (_ should equal (3.0))

    val vdz: DenseVectorRow[Double] = DenseVectorRow.zeros[Double](42)
    vdz.length should equal (42)
    vdz.values foreach (_ should equal (0))

    // TODO: Insert test for zeros and non-primitive types. But -- are there
    // currently any non-primitive scalars at all?

    val vbo: DenseVectorRow[Boolean] = DenseVectorRow.ones[Boolean](8)
    vbo.length should equal (8)
    vbo.values foreach (_ should equal (true))

    val vit: DenseVectorRow[Int] = DenseVectorRow.tabulate[Int](3)(_ * 3)
    vit.length should equal (3)
    (vit.values zip (0 to 6 by 3)) foreach { case (a,b) => a should equal (b) }

    val vir: DenseVectorRow[Int] = DenseVectorRow.range(0, 100, 10)
    vir.length should equal(10)
    (vir.values zip (0 to 100 by 10)) foreach { case (a,b) => a should equal (b) }

    val mt = new MersenneTwisterFast(0l)

    val vdr: DenseVectorRow[Double] = DenseVectorRow.rand(5, mt)
    (vdr.values zip Seq(0.548, 0.715, 0.602, 0.544, 0.423)) foreach
      { case (a,b) => a should be (b plusOrMinus 1e-3) }

    val vdrn: DenseVectorRow[Double] = DenseVectorRow.randn(5, mt)
    (vdrn.values zip Seq(1.969, -0.842, -0.505, 1.266, 1.080)) foreach
      { case (a,b) => a should be (b plusOrMinus 1e-3) }

    val viri: DenseVectorRow[Int] = DenseVectorRow.randi(1888, 5, mt)
    (viri.values zip Seq(242, 140, 262, 644, 388)) foreach
      { case (a,b) => a should equal (b) }

    val vhc: DenseVectorRow[Double] = DenseVectorRow.horzcat[Double](vd, vd, vd)
    (vhc.values zip Seq(1,2,3,1,2,3,1,2,3)) foreach
      { case (a,b) => a should equal (b) }

    val vvc: DenseMatrix[Int] =
      DenseVectorRow.vertcat[Int](DenseVectorRow(1,2,3),
                                  DenseVectorRow(4,5,6),
                                  DenseVectorRow(7,8,9))
    vvc foreachPair { case ((i,j), v) => v should equal (i*3+j+1) }

    evaluating {
        // Incompatible lengths
        DenseVectorRow.vertcat(DenseVectorRow(1,2), DenseVectorRow(4,5,6))
    } should produce [IllegalArgumentException]
  }

  test("DenseVectorCol") {
    val vd: DenseVectorCol[Double] = DenseVectorCol(1.0, 2.0, 3.0)
    vd.length should equal (3)
    (vd.values zip (1 to 3)) foreach { case (a,b) => a should equal (b) }

    val vf: DenseVectorCol[Float] = DenseVectorCol(Array(1.0f, 2.0f, 3.0f))
    vf.length should equal (3)
    (vf.values zip (1 to 3)) foreach { case (a,b) => a should equal (b) }

    val vdomi: DenseVectorCol[Int] = DenseVectorCol[Int](IndexDomain(10))
    vdomi.length should equal (10)
    vdomi.values foreach (_ should equal (0))

    val vdf: DenseVectorCol[Double] = DenseVectorCol.fill(23)(3.0)
    vdf.length should equal (23)
    vdf.values foreach (_ should equal (3.0))

    val vdz: DenseVectorCol[Double] = DenseVectorCol.zeros[Double](42)
    vdz.length should equal (42)
    vdz.values foreach (_ should equal (0))

    // TODO: Insert test for zeros and non-primitive types. But -- are there
    // currently any non-primitive scalars at all?

    val vbo: DenseVectorCol[Boolean] = DenseVectorCol.ones[Boolean](8)
    vbo.length should equal (8)
    vbo.values foreach (_ should equal (true))

    val vit: DenseVectorCol[Int] = DenseVectorCol.tabulate[Int](3)(_ * 3)
    vit.length should equal (3)
    (vit.values zip (0 to 6 by 3)) foreach { case (a,b) => a should equal (b) }

    val vir: DenseVectorCol[Int] = DenseVectorCol.range(0, 100, 10)
    vir.length should equal(10)
    (vir.values zip (0 to 100 by 10)) foreach { case (a,b) => a should equal (b) }

    val mt = new MersenneTwisterFast(0l)

    val vdr: DenseVectorCol[Double] = DenseVectorCol.rand(5, mt)
    (vdr.values zip Seq(0.548, 0.715, 0.602, 0.544, 0.423)) foreach
      { case (a,b) => a should be (b plusOrMinus 1e-3) }

    val vdrn: DenseVectorCol[Double] = DenseVectorCol.randn(5, mt)
    (vdrn.values zip Seq(1.969, -0.842, -0.505, 1.266, 1.080)) foreach
      { case (a,b) => a should be (b plusOrMinus 1e-3) }

    val viri: DenseVectorCol[Int] = DenseVectorCol.randi(1888, 5, mt)
    (viri.values zip Seq(242, 140, 262, 644, 388)) foreach
      { case (a,b) => a should equal (b) }

    val vhc: DenseVectorCol[Double] = DenseVectorCol.vertcat[Double](vd, vd, vd)
    (vhc.values zip Seq(1,2,3,1,2,3,1,2,3)) foreach
      { case (a,b) => a should equal (b) }

    val vvc: DenseMatrix[Int] =
      DenseVectorCol.horzcat[Int](DenseVectorCol(1,4,7),
                                  DenseVectorCol(2,5,8),
                                  DenseVectorCol(3,6,9))
    vvc foreachPair { case ((i,j), v) => v should equal (i*3+j+1) }

    evaluating {
        // Incompatible lengths
        DenseVectorCol.horzcat(DenseVectorCol(1,2), DenseVectorCol(4,5,6))
    } should produce [IllegalArgumentException]
  }

}
