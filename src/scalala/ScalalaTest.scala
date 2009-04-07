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
 * A simple, lightweight testing framework inspired by SUnit but with
 * substantially less overhead.  Any Object that extends the inner
 * TestConsoleMain class will automatically have all methods whose
 * name ends in ("_test") called.  Each of these methods may use
 * any of the assertion methods in this object.
 * 
 * @author dramage
 */
object ScalalaTest {
  
  import scalala.collection.PartialMap;
  
  val TOLERANCE = 1e-4;
  
  case class AssertionException(message : String) extends Exception;
  
  def assertEquals[V1,V2](v1 : =>V1, v2 : =>V2) : Unit =
    if (v1 != v2) throw new AssertionException(v1 + "!=" + v2);
  
  def assertEquals(v1 : =>Double, v2 : =>Double, tolerance : Double) : Unit =
    if (Math.abs(v1 - v2) > tolerance) throw new AssertionException(v1 + "!=" + v2);
  
  def assertEquals[I](v1 : PartialMap[I,Double], v2 : PartialMap[I,Double], tolerance : Double) : Unit =
    if ((v1 join v2)((a:Double,b:Double) => Math.abs(a - b) < tolerance).values.contains(false))
      throw new AssertionException(v1 + "!=" + v2);
  
  /** Two-arg function application with expected exception. */
  def assertThrows[V](f : =>V, exType : java.lang.Class[_ <: java.lang.Throwable]) : Unit = {
    def die() = throw AssertionException("Evaluation did not throw "+exType+": "+f.toString);
    try {
      val x = f.toString;
      // should have thrown but didn't
      die();
    } catch {
      case thrown : Exception => {
        if (thrown.getClass != exType) {
          // threw something else
          die();
        }
      }
    }
    // threw the right thing.  return without issue.
  }
  
  /** Hook for doing tests in a class by reflection. @author dramage */
  trait TestConsoleMain {
    def main(argv : Array[String]) {
      for (method <- getClass.getMethods if (method.getName().endsWith("_test"))) {
        print(getClass.getName+"."+method.getName()+": ");
        try {
          method.invoke(this);
          println("OK");
        } catch {
          case ite : java.lang.reflect.InvocationTargetException => {
            ite.getCause match {
              case ae : AssertionException => {
                println("FAILED "+ae.message);
                ae.printStackTrace();
              }
              case oe : Throwable => {
                println("FAILED "+oe);
                oe.printStackTrace();
              }
            }
          }
          case oe : Exception => println("FRAMEWORK FAILURE "+oe);
        }
      }
    }
  }
}
