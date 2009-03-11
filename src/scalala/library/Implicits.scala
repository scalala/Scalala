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
package scalala.library

import scalala.ScalalaValueException;

trait Implicits extends Library {
  
  //
  // Type promotions
  //

  implicit def iDenseMatrixFromSeqSeq[T<:AnyVal](data : Seq[Seq[T]]) : Matrix = {
    val numRows = data.length;
    val numCols = data map (_.length) reduceLeft Math.max;
    val matrix  = DenseMatrix(numRows, numCols);
    for (i <- 0 until data.length) {
      val seq = data(i);
      if (seq.length >= 1) {
             if (seq(0).isInstanceOf[Double]) { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Double]; }
        else if (seq(0).isInstanceOf[Float])  { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Float]; }
        else if (seq(0).isInstanceOf[Int])    { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Int]; }
        else if (seq(0).isInstanceOf[Long])   { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Long]; }
        else if (seq(0).isInstanceOf[Short])  { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Short]; }
        else if (seq(0).isInstanceOf[Byte])   { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Byte]; }
        else throw new ScalalaValueException("Unrecognized numeric type in sequence promotion");
      }
    }
    return matrix
  }
  
  implicit def iDenseVectorFromDoubleArray(array : Array[Double]) : Vector = {
    DenseVector(array);
  }
  
  implicit def iDenseVectorFromSeq[T<:AnyVal](seq : Seq[T]) : Vector = {
    if (seq.isInstanceOf[Array[Double]]) {
      // special case for Array[Double] as a pass-through
      return iDenseVectorFromDoubleArray(seq.asInstanceOf[Array[Double]]);
    }
    
    val v = DenseVector(seq.length);
    if (seq.length >= 1) {
           if (seq(0).isInstanceOf[Double]) { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Double]; }
      else if (seq(0).isInstanceOf[Float])  { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Float]; }
      else if (seq(0).isInstanceOf[Int])    { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Int]; }
      else if (seq(0).isInstanceOf[Long])   { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Long]; }
      else if (seq(0).isInstanceOf[Short])  { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Short]; }
      else if (seq(0).isInstanceOf[Byte])   { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Byte]; }
      else throw new ScalalaValueException("Unrecognized numeric type in sequence promotion");
    }
    return v;
  }
}
