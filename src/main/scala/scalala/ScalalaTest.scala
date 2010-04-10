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
package scalala

/**
 * A simple, lightweight testing framework inspired by ScalaCheck's
 * FunSuite but with partial-map support.
 * 
 * @author dramage
 */
trait ScalalaTest {
  import ScalalaTest._;
  import scalala.collection.PartialMap;

  /** List of test functions. */
  protected var tests = List[(String, ()=>Unit)]();
  
  /** Registers a test function. */
  protected def test(name : String)(func : => Unit) =
    tests ::= (name, func _);
  
  def assertEquals[V1,V2](v1 : =>V1, v2 : =>V2) : Unit =
    if (v1 != v2) throw new TestFailedException(v1 + "!=" + v2);
  
  def assertEquals(v1 : =>Double, v2 : =>Double, tolerance : Double) : Unit =
    if (Math.abs(v1 - v2) > tolerance) throw new TestFailedException(v1 + "!=" + v2);
  
  def assertEquals[I](v1 : PartialMap[I,Double], v2 : PartialMap[I,Double], tolerance : Double) : Unit =
    if ((v1 join v2)((a:Double,b:Double) => Math.abs(a - b) < tolerance).valuesIterator.contains(false))
      throw new TestFailedException(v1 + "!=" + v2);

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
}

object ScalalaTest {
  case class TestFailedException(message : String) extends Exception;
  
  /** Hook for doing tests in a class by reflection. @author dramage */
  trait TestConsoleMain extends ScalalaTest {
  
    /** Main method for running all test functions. */ 
    def main(args : Array[String]) {
      for ((name,func) <- tests) {
        print(name+": ");
        try {
          func();
          println("PASSED");
        } catch {
          case ae : TestFailedException => {
            println("FAILED "+ae.message);
            ae.printStackTrace();
          }
          case oe : Throwable => {
            println("ERROR "+oe);
            oe.printStackTrace();
          }
        }
      }
    }
  }
}

object TestConsoleMain extends ScalalaTest.TestConsoleMain;
