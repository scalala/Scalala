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
package tensor;
package dense;

import collection.{MergeableSet, ProductSet, IntSpanSet};
import Tensor.CreateException;
import operators._;
import TensorShapes._;

/**
 * A matrix backed by a dense array of doubles, with each column stored
 * before the next column begins.
 * 
 * @author dramage
 */
class DenseMatrix(nRows : Int, nCols : Int, data : Array[Double]) extends
  DoubleArrayData(data) with Matrix with DenseTensor[(Int,Int)]
  with TensorSelfOp[(Int,Int),DenseMatrix,Shape2] {
  
  if (nRows * nCols != data.length)
    throw new IllegalArgumentException("data.length must equal nRows*nCols");
  
  def this(nRows : Int, nCols : Int) =
    this(nRows, nCols, new Array[Double](nRows * nCols));
  
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

  private val _rowDomain = IntSpanSet(0, cols);
  override final def activeDomainInRow(row : Int) = _rowDomain;
  
  private val _colDomain = IntSpanSet(0, rows);
  override def activeDomainInCol(col : Int) = _colDomain;
  
  override def copy = {
    val arr = new Array[Double](rows * cols);
    System.arraycopy(data,0,arr,0,size);
    new DenseMatrix(rows, cols, arr);
  }

  override def like = new DenseMatrix(rows, cols);
  
  override def zero = java.util.Arrays.fill(data, 0.0);
  
  private def log(msg : String) =
    System.err.println("DenseMatrix: "+msg);
  
  override def toString() = {
    def formatInt(x : Double) : String = {
      if (x.isPosInfinity)
        " Inf"
      else if (x.isNegInfinity) 
        "-Inf"
      else if (x.isNaN)
        " NaN"
      else
        x.toInt.toString;
    }

    def formatDouble(x : Double) : String = {
      if (x == 0)
        "      0"
      else if (x.isPosInfinity)
        "    Inf"
      else if (x.isNegInfinity) 
        "   -Inf"
      else if (x.isNaN)
        "    NaN"
      else
        String.format("% 4.4f", double2Double(x));
    }
    
    val (prefix,format) = {
      if (data.iterator.forall(x => x.isNaN || x.isInfinite || x == x.floor)) {
        // special case for ints
        ("", formatInt _);
      } else {
        val maxlog = scalala.Scalala.max(for (value <- data; if !value.isInfinite && !value.isNaN) yield Math.log(value));
        val exponent = ((maxlog / Math.log(10)) + 1e-3).toInt;
        if (Math.abs(exponent) >= 3) {
          // special case for very large or small numbers
          val scale = Math.pow(10,exponent);
          ("  1.0e"+(if (exponent >= 0) "+" else "") + exponent+" * \n\n",
           ((x:Double) => formatDouble(x / scale)));
        } else {
          // general case
          ("", formatDouble _);
        }
      }
    }
    
    def colWidth(col : Int) : Int =
      Math.max(4,(0 until rows).map((row:Int) => format(this(row,col)).length).reduceLeft(Math.max));
    
    val columnWidths = (0 until nCols).map(colWidth).toArray;
    
    val builder = for (row <- 0 until rows; col <- 0 until cols) yield {
      val element = format(this(row,col));
      "  " + (" " * (columnWidths(col)-element.length)) + element + (if (col == cols-1) "\n" else "");
    }
    
    (List(prefix).iterator ++ builder.iterator).mkString("");
  }
}

object DenseMatrix {
  /**
   * Static constructor that creates a dense matrix of the given size
   * initialized by iterator from the given values list (looping if
   * necessary).  The values are initialized column-major, i.e. values
   * is read in order to populate the matrix, filling up column 0 before
   * column 1, before column 2 ...
   */
  def apply(rows : Int, cols : Int)(values : Double*) = {
    new DenseMatrix(rows, cols, Array.tabulate(rows * cols)(i => values(i % values.length)));
  }
}
