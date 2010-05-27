/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala;

import org.scalatest._;
import org.scalatest.prop._;
import org.scalacheck._;

import scalala.library.Operators
import scalala.library.Random
import scalala.tensor.dense._;

/**
 * Scalala specific test support, extending ScalaTest.
 * 
 * @author dramage
 */
trait ScalalaTest extends FunSuite with Checkers with Random with Operators {
  import ScalalaTest._;
  import collection.PartialMap;

  def assertEquals[V1,V2](v1 : =>V1, v2 : =>V2) : Unit =
    if (v1 != v2) throw new TestFailedException(v1 + "!=" + v2);

  /**
   * Actually returns true or throws.
   */
  def assertEquals(v1 : =>Double, v2 : =>Double, tolerance : Double) : Boolean =
     (math.abs(v1 - v2) < tolerance) || {throw new TestFailedException(v1 + "!=" + v2)};
  
  /**
   * Actually returns true or throws.
   */
  def assertEquals[I](v1 : PartialMap[I,Double], v2 : PartialMap[I,Double], tolerance : Double) : Boolean =
     (!((v1 join v2)((a:Double,b:Double) => math.abs(a - b) < tolerance).valuesIterator.contains(false))
      || {throw new TestFailedException(v1 + "!=" + v2)});

  /** Expects the exception named in the type arg to be thrown. */
  def assertThrows[T <: AnyRef](func : => Any)(implicit manifest : scala.reflect.Manifest[T]) : T = {
    def die() = throw TestFailedException("Evaluation did not throw "+manifest+": "+func.toString);
    try {
      val x = func;
      // should have thrown but didn't
      die();
    } catch {
      case thrown : Throwable if (thrown.getClass == manifest.erasure) =>
        return thrown.asInstanceOf[T];
      case _ =>
        die();
    }
    // threw the right thing.  return without issue.
  }

  implicit val arbDenseMatrix: Arbitrary[DenseMatrix] = Arbitrary {
    for {
      n <- Arbitrary.arbitrary[Int].map(_.abs % 100) if n > 0;
      m <- Arbitrary.arbitrary[Int].map(_.abs % 100) if m > 0;
      scale <- Arbitrary.arbitrary[Double]
    } yield (randn(m,n) * scale) value;
  }

  implicit val arbDenseVector: Arbitrary[DenseVector] = Arbitrary {
    for {
      n <- Arbitrary.arbitrary[Int].map(_.abs % 1000);
      scale <- Arbitrary.arbitrary[Double]
    } yield (randn(n) * scale) value;
  }
}

object ScalalaTest {
  case class TestFailedException(message : String) extends Exception;
}