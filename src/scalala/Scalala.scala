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

import scalala.library._;

/**
 * A matlab-like environment and syntax for scala.  This is the default
 * library, using the MTJ back-end and all modules in the scalala.library
 * package.  The companion object inherits this trait, so all of Scalala
 * can be imported with the single line import scalala.Scalala._;
 * 
 * @author dramage
 */
trait Scalala extends Library
  with Implicits
  with Operators
  with Matrices
  with Vectors
  with Plotting
  with Random
  with Statistics
  with IO
{
  // This object inherits all its method from the component mixins.
}

/**
 * A matlab-like environment and syntax for scala.  This is the default
 * library, using the MTJ back-end and all modules in the scalala.library
 * package.  All of Scalala can be imported direclty with
 * import scalala.Scalala._;
 * 
 * @author dramage
 */
object Scalala extends Scalala
{
  // This object inherits all its method from the component mixins.
}

/**
 * The global test suite.
 */
object ScalalaTestSuite extends Scalala
  with scalala.tensor.operators.OperatorTest
  with StatisticsTest
  with ScalalaTest.TestConsoleMain
{
  // Test suite
}
