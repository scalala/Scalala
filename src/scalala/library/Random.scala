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
package scalala.library;

import scalala.tensor.{Tensor,Vector,Matrix};

import scalala.tensor.dense.{DenseVector,DenseMatrix};

/**
 * Random number generation.
 * 
 * @author dramage
 */
trait Random extends Library {
  implicit val _scalala_random = new java.util.Random;
  
  /** Returns a psuedo-random number from the interval 0 to 1 */
  def rand()(implicit rand : java.util.Random) = rand.nextDouble;
  
  /** Returns vector of size n, each element from 0 to 1 */
  def rand(n : Int)(implicit rand : java.util.Random) : Vector = {
    val v = new DenseVector(n);
    for (i <- 0 until n) {
      v(i) = rand.nextDouble;
    }
    return v;
  }
  
  /** Returns a random matrix of the given size, each element drawn from 0 to 1 */
  def rand(rows : Int, cols : Int)(implicit rand : java.util.Random) : Matrix = {
    val m = new DenseMatrix(rows,cols);
    for (i <- 0 until rows; j <- 0 until cols) {
      m(i,j) = rand.nextDouble;
    }
    return m;
  }
  
  /** Returns a pseudo-random gaussian variable */
  def randn()(implicit rand : java.util.Random) = rand.nextGaussian;
  
  /** Returns a vector of size n, each element from a gaussian*/
  def randn(n : Int)(implicit rand : java.util.Random) : Vector = {
    val v = new DenseVector(n);
    for (i <- 0 until n) {
      v(i) = rand.nextGaussian;
    }
    return v;
  }
  
  /** Returns a random matrix of the given size, each element drawn from a gaussian */
  def randn(rows : Int, cols : Int)(implicit rand : java.util.Random) : Matrix = {
    val m = new DenseMatrix(rows,cols);
    for (i <- 0 until rows; j <- 0 until cols) {
      m(i,j) = rand.nextGaussian;
    }
    return m;
  }
}
