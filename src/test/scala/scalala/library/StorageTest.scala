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
import scalala.tensor.dense.DenseMatrix
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}


@RunWith(classOf[JUnitRunner])
class StorageTest extends FunSuite with Checkers with ShouldMatchers {

  test("StoreTxtAndLoadTxt") {
    val os = new ByteArrayOutputStream

    val m = DenseMatrix.randn(5,5)
    Storage.storetxt(os, m)
    val n = Storage.loadtxt(new ByteArrayInputStream(os.toByteArray))
    m foreachPair ((idx,v) => n(idx) should be (v plusOrMinus 1e-16))

    os.reset()
    Storage.storetxt(os, m)
    val o = Storage.loadtxt(new ByteArrayInputStream(os.toByteArray), skipRows=2)
    o.numRows should be (m.numRows-2)
    o foreachPair ((idx,v) => m(idx._1+2,idx._2) should be (v plusOrMinus 1e-16))

    os.reset()
    Storage.storetxt(os, m)
    val p = Storage.loadtxt(new ByteArrayInputStream(os.toByteArray), columns=Seq(1,3))
    p.numCols should be (2)
    p foreachPair {
      case ((row,0), v) => m(row,1) should be (v plusOrMinus 1e-16)
      case ((row,1), v) => m(row,3) should be (v plusOrMinus 1e-16)
      case _ => true should be (false) // signal failure
    }

    val is = new ByteArrayInputStream("1.0 2.0 #test\n#foo\n3.0 4.0".getBytes)
    val r = Storage.loadtxt(is)
    r.numRows should be (2)
    r.numCols should be (2)
    r(0,0) should be (1.0)
    r(0,1) should be (2.0)
    r(1,0) should be (3.0)
    r(1,1) should be (4.0)
  }

}
