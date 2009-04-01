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
package scalala.tensor.dense;

import scalala.tensor.{Tensor,Matrix};
import scalala.collection.domain.{Domain, Domain2, IntSpanDomain};

import scalala.tensor.Tensor.CreateException;

import scalala.tensor.spans.IntSpans._;

/**
 * A vector backed by a dense array of doubles, with each column stored
 * before the next column begins.
 * 
 * @author dramage
 */
class DenseMatrix(data : Array[Double], nRows : Int, nCols : Int) extends DoubleArrayData(data) with Matrix {
  if (nRows * nCols != data.length) throw new Predef.IllegalArgumentException;
  
  def this(nRows : Int, nCols : Int) = this(new Array[Double](nRows * nCols), nRows, nCols);
  
  @inline final def index(row : Int, col : Int) : Int = {
    check(row,col);
    row + col * rows;
  }
  
  override def rows = nRows;
  override def cols = nCols;
  
  override def apply(row : Int, col : Int) : Double =
    data(index(row,col));
  
  override def update(row : Int, col : Int, value : Double) =
    data(index(row,col)) = value;

  /** Cannot change default value for dense tensors. */
  override def default_=(update : Double) = {};
  
  override lazy val activeDomain : Set[(Int,Int)] = (0,0) until (rows,cols);
  
  override def copy = new DenseMatrix(data.toArray, rows, cols).asInstanceOf[this.type];
    
  override def create[J](d : Domain[J]) : Tensor[J] = d match {
    case Domain2(rowD,colD) =>
      (rowD,colD) match {
        case (IntSpanDomain(0,rows),IntSpanDomain(0,cols)) =>
          new DenseMatrix(new Array[Double](rows*cols), rows, cols);
        case _ => throw new CreateException("Invalid domain for DenseMatrix construction.");
      };
    case _ => throw new CreateException("Invalid domain for DenseMatrix construction.");
  }
  
  override def zero() = {
    this.default = 0;
    java.util.Arrays.fill(data, 0.0);
  }
}
