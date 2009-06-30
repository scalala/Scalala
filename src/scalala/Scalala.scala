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
 * Global object for importing the entire Scalala environment.
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
  with scalala.tensor.sparse.SparseHashVectorTest
  with scalala.tensor.sparse.SparseBinaryVectorTest
  with scalala.tensor.sparse.SingletonBinaryVectorTest
  with VectorsTest
  with StatisticsTest
  with ScalalaTest.TestConsoleMain
{
  // This object inherits all its method from the component mixins.
}


/**
 * The global profiling suite.
 * 
 * @author dramage
 */
object ScalalaProfilingSuite extends Scalala
  with ScalalaProfiling
  with ScalalaTest.TestConsoleMain
{
    
//  test("SparseBinaryVectorMultAdd") {
//    import scalala.tensor.dense._;
//    import scalala.tensor.sparse._;
//    
//    val iter = 100;
//    val n = 50000;
//    
//    val direct = profile(iter) {
//      val dv = new DenseVector(n);
//      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
//      for(i <- sv.activeDomain) {
//        dv(i) += sv(i) * 1.0
//      }
//    }
//    
//    val operator = profile(iter) {
//      val dv = new DenseVector(n);
//      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
//      dv += sv * 1.0;
//    }
//    
//    printf("%g %g\n", direct, operator);
//  }
  
  test("SparseBinaryVectorActiveDomain") {
    import scalala.tensor.sparse._;
    import scalala.collection.MergeableSet;

    val iter = 2000;
    val n = 5000;

    val direct = profile(iter) {
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      for(i<- 0 until iter)
        for(idx <- sv.index)
          0;
    }

    def activeDomain(sv:SparseBinaryVector) = new MergeableSet[Int] {
      override def size = sv.used;
      override def contains(i : Int) = sv.findOffset(i) >= 0;
      override def elements =
        if (sv.used==sv.index.length) sv.index.elements;
          else sv.index.take(sv.used).elements;
    }

    val indirect = profile(iter) {
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      for(i<- 0 until iter)
        for(idx <- activeDomain(sv))
          0;
    }

    val default = profile(iter) {
      val sv = new SparseBinaryVector(n,Array(1,20,300,4000,4500,4999));
      for(i<- 0 until iter)
        for(idx <- sv.activeDomain)
          0;
    }

    printf("%g %g %g\n", direct, indirect, default);
  }
  
  test("SparseVector vs SparseHashVector") {
    import scalala.tensor.sparse._;
    
    val iter = 100;
    val n = 50000;
    
    println();
    
    for (m <- 100 to 5000 by 500) {
      val profileSparseVector = profile(iter) {
        val rand = new java.util.Random(m);
        val x = new SparseVector(n);
        var i = 0;
        while (i < m) {
          x(rand.nextInt(n)) = rand.nextDouble;
          i += 1;
        }
      }

      val profileInt2DoubleCounter = profile(iter) {
        val rand = new java.util.Random(m);
        val x = new SparseHashVector(n);
        var i = 0;
        while (i < m) {
          x(rand.nextInt(n)) = rand.nextDouble;
          i += 1;
        }
      }
      
      printf("% 5d %01.6f %01.6f\n", m, profileSparseVector, profileInt2DoubleCounter);
    }
  }
}
