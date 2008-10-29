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
  
  val TOLERANCE = 1e-4;
  
  case class AssertionException(message : String) extends Exception;
  
  def assertEquals[V1,V2](v1 : V1, v2 : V2) : Unit =
    if (v1 != v2) throw new AssertionException(v1 + "!=" + v2);
  
  /** Two-arg function application with expected output. */
  def assertEquals[IN1,IN2,OUT](f : ((IN1,IN2) => OUT), in1 : IN1, in2 : IN2, out : OUT, valid : ((OUT,OUT) => Boolean)) : Unit =
    if (!valid(f(in1,in2),out)) throw AssertionException(f.toString + "(" + in1 + "," + in2 + ") != "+out);
  
  /** Two-arg function application with "==" equality on outputs. */
  def assertEquals[IN1,IN2,OUT<:AnyRef](f :((IN1,IN2) => OUT), in1 : IN1, in2 : IN2, out : OUT) : Unit =
    assertEquals(f, in1, in2, out, ((a:OUT, b:OUT) => a == b)); 
  
  /** Two-arg function application with "eq" equality on outputs. */
  def assertEq[IN1,IN2,OUT<:AnyRef](f :((IN1,IN2) => OUT), in1 : IN1, in2 : IN2, out : OUT) : Unit =
    assertEquals(f, in1, in2, out, ((a:OUT, b:OUT) => a eq b)); 
  
  /** Two-arg function application with numerical return value and tolerance. */
  def assertEquals[IN1,IN2](f : ((IN1,IN2) => Double), in1 : IN1, in2 : IN2, out : Double, tolerance : Double) : Unit =
    assertEquals(f, in1, in2, out, (a:Double, b:Double) => Math.abs(a-b) < tolerance);
  
  /** Two-arg function application with numerical return value and default tolerance of 1e-6. */
  def assertEquals[IN1,IN2](f : ((IN1,IN2) => Double), in1 : IN1, in2 : IN2, out : Double) : Unit =
    assertEquals(f, in1, in2, out, TOLERANCE);
  
  /** Two-arg function application with numerical return value and tolerance. */
  def assertEquals[IN1,IN2](f : ((IN1,IN2) => Float), in1 : IN1, in2 : IN2, out : Float, tolerance : Float) : Unit =
    assertEquals(f, in1, in2, out, (a:Float, b:Float) => Math.abs(a-b) < tolerance);
  
  /** Two-arg function application with numerical return value and default tolerance of 1e-6f. */
  def assertEquals[IN1,IN2](f : ((IN1,IN2) => Float), in1 : IN1, in2 : IN2, out : Float) : Unit =
    assertEquals(f, in1, in2, out, TOLERANCE.asInstanceOf[Float]);
  
  /** Two-arg function application with expected exception. */
  def assertThrows[IN1,IN2,OUT](f : ((IN1,IN2) => OUT), in1 : IN1, in2 : IN2, exType : java.lang.Class[_ <: java.lang.Throwable]) : Unit = {
    def die() = throw AssertionException(f.toString + "("+in1+","+in2+") did not throw "+exType);
    try {
      f(in1, in2);
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
  
  /** Hook for doing tests in a class by reflection. */
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
              case ae : AssertionException => println("FAILED "+ae.message);
              case oe : Exception => println("FAILED "+oe);
            }
          }
          case oe : Exception => println("FRAMEWORK FAILURE "+oe);
        }
      }
    }
  }
}


object ScalalaTestSuite {
  val tests = List("Scalala","ScalalaOpsTest","ScalalaValuesTest")
  
  def main(argv : Array[String]) {
    for (c <- tests) {
      Class.forName("scalala."+c).getMethod("main",classOf[Array[String]]).invoke(null,Array(Array[String]()));
    }
  }
}
