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
 * library, using all modules in the scalala.library package.  The
 * companion object inherits this trait, so all of Scalala
 * can be imported with the single line import scalala.Scalala._;
 * 
 * Where a function shares its name with a Matlab function, the
 * behavior should be more or less consist with its Matlab
 * counterpart. The primary difference is that some commands that normally
 * return an n x n square matrix now return a column vector of size n. e.g.
 * ones(n) here is a vector that would be ones(n,1) in matlab.  The
 * main reason for the distinction is that Vector and Matrix are
 * different types in MTJ, so ones(n,1) returns a Matrix of size n by 1
 * which shouldn't need its own conversion back to a vector.
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
 * library, using all modules in the scalala.library package.  All of
 * Scalala can be imported direclty with import scalala.Scalala._;
 * 
 * Where a function shares its name with a Matlab function, the
 * behavior should be more or less consist with its Matlab
 * counterpart. The primary difference is that some commands that normally
 * return an n x n square matrix now return a column vector of size n. e.g.
 * ones(n) here is a vector that would be ones(n,1) in matlab.  The
 * main reason for the distinction is that Vector and Matrix are
 * different types in MTJ, so ones(n,1) returns a Matrix of size n by 1
 * which shouldn't need its own conversion back to a vector.
 * 
 * @author dramage
 */
object Scalala extends Scalala
{
  // This object inherits all its method from the component mixins.
}

/**
 * The global test suite.
 * 
 * @author dramage
 */
object ScalalaTestSuite extends Scalala
  with scalala.collection.MergeableSetTest
  with scalala.tensor.operators.OperatorTest
  with scalala.tensor.sparse.SparseVectorTest
  with scalala.tensor.dense.DenseMatrixSolveTest
  with StatisticsTest
  with ScalalaTest.TestConsoleMain
{
  // Test suite
}
