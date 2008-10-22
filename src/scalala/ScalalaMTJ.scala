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

import no.uib.cipr.matrix.{Matrices => MTJMatrices}
import no.uib.cipr.matrix.{Matrix => MTJMatrix,MatrixEntry => MTJMatrixEntry}
import no.uib.cipr.matrix.{DenseMatrix => MTJDenseMatrix, AbstractMatrix => MTJAbstractMatrix}
import no.uib.cipr.matrix.{Vector => MTJVector, VectorEntry => MTJVectorEntry, DenseVector => MTJDenseVector, AbstractVector => MTJAbstractVector}
import no.uib.cipr.matrix.sparse.{SparseVector => MTJSparseVector,CompColMatrix => MTJCompColMatrix}

/**
 * MTJ compatibility wrappers for Scalala matrix types.
 */
object ScalalaMTJ {

  //
  // Basic data type constructors
  //
    
  def DenseMatrix(rows : Int, cols : Int) : Matrix =
    new MTJDenseMatrix(rows,cols);
  
  def DenseVector(size : Int) : Vector =
    new MTJDenseVector(size);
  
  def DenseVector(values : Array[Double]) : Vector =
    new MTJDenseVector(values, false);
  
  def SparseVector(size : Int) : Vector =
    new MTJSparseVector(size, Math.min(size/10,1000));
  
  /** Returns an MTJMatrix for the given Matrix */
  def MTJMatrix[M<:MTJMatrix](wrapped : MTJMatrixWrapper[M]) : M =
    wrapped.matrix;
  
  /** Returns an MTJMatrix for the given Matrix */
  def MTJMatrix[M<:MTJMatrix](matrix : Matrix) : MTJMatrix = {
    if (matrix.isInstanceOf[MTJMatrixWrapper[M]]) {
      matrix.asInstanceOf[MTJMatrixWrapper[M]].matrix;
    } else {
      new MTJAbstractMatrix(matrix.rows, matrix.cols) {
        override def get(row : Int, col : Int) = matrix.get(row,col);
        override def set(row : Int, col : Int, x : Double) = matrix.set(row,col,x);
        override def iterator : java.util.Iterator[MTJMatrixEntry] = {
          val iter = matrix.elements;
          
          var _entry : MatrixEntry = null;
          
          val entry = new MTJMatrixEntry {
            override def row = _entry.row;
            override def column = _entry.col;
            override def get = _entry.get;
            override def set(x: Double) = _entry.set(x);
            override def toString = _entry.toString;
          }
          
          return new java.util.Iterator[MTJMatrixEntry] {
            override def hasNext = iter.hasNext;
            override def next = {
              _entry = iter.next;
              entry;
            }
            override def remove =
              throw new UnsupportedOperationException;
          }
        }
      }
    }
  }
  
  def MTJVector[V<:MTJVector](wrapped : MTJVectorWrapper[V]) : V =
    wrapped.vector;
  
  def MTJVector[V<:MTJVector](vector : Vector) : MTJVector = {
    if (vector.isInstanceOf[MTJVectorWrapper[V]]) {
      vector.asInstanceOf[MTJVectorWrapper[V]].vector;
    } else {
      new MTJAbstractVector(vector.size) {
        override def get(index : Int) = vector.get(index);
        override def set(index : Int, value : Double) = vector.set(index,value);
        override def iterator : java.util.Iterator[MTJVectorEntry] = {
          val iter = vector.elements;
          
          var _entry : VectorEntry = null;
          
          val entry = new MTJVectorEntry {
            override def index = _entry.index;
            override def get = _entry.get;
            override def set(x : Double) = _entry.set(x);
            override def toString = _entry.toString;
          }
          
          return new java.util.Iterator[MTJVectorEntry] {
            override def hasNext = iter.hasNext;
            override def next = {
              _entry = iter.next;
              entry;
            }
            override def remove =
              throw new UnsupportedOperationException;
          }
        }
      }
    }
  }
  
  //
  // wrapper classes
  //
  
  /** Implicit promotion of MTJMatrix to a wrapped version */
  implicit def ScalalaMatrix[M<:MTJMatrix](matrix : M) : MTJMatrixWrapper[M] =
    new MTJMatrixWrapper(matrix);
  
  /** Wraps an underlying MTJMatrix as a Matrix */
  class MTJMatrixWrapper[M<:MTJMatrix](val matrix : M) extends Matrix {
    override def rows = matrix.numRows;
    override def cols = matrix.numColumns;
    
    override def get(row : Int, col : Int) : Double =
      matrix.get(row,col);
    
    override def set(row : Int, col : Int, value : Double) =
      matrix.set(row,col,value);
    
    override def elements : Iterator[MatrixEntry] = {
      val iter = matrix.iterator;
      var _entry : MTJMatrixEntry = null;
      
      val entry = new MatrixEntry {
        override def row = _entry.row;
        override def col = _entry.column;
        override def get = _entry.get;
        override def set(x : Double) = _entry.set(x);
      }
      
      new Iterator[MatrixEntry] {
        override def hasNext = iter.hasNext;
        override def next = {
          _entry = iter.next;
          entry;
        }
      };
    }
  }
  
  /** Implicit promotion of MTJVector to a wrapped version */
  implicit def ScalalaVector[V<:MTJVector](vector : V) : MTJVectorWrapper[V] =
    new MTJVectorWrapper(vector);
  
  /** Wraps an underlying MTJVector as a Vector */
  class MTJVectorWrapper[V<:MTJVector](val vector : V) extends Vector {
    override def size = vector.size;
    
    override def get(i : Int) : Double = vector.get(i);
    
    override def set(i : Int, value : Double) = vector.set(i, value);
    
    override def elements : Iterator[VectorEntry] = {
      val iter = vector.iterator;
      var _entry : MTJVectorEntry = null;
      
      val entry = new VectorEntry {
        override def index = _entry.index;
        override def get   = _entry.get;
        override def set(x : Double) = _entry.set(x);
      }
      
      new Iterator[VectorEntry] {
        override def hasNext = iter.hasNext;
        override def next = {
          _entry = iter.next;
          entry;
        }
      }
    }
  }
}  

object RichMTJ {
  /** The given scalar as an immutable matrix of the given size */
  case class ScalarMatrix(value : Double, rows : Int, cols : Int) extends MTJAbstractMatrix(rows,cols) {
    override def get(row : Int, col : Int) : Double = {
      check(row,col)
      return value
    }
    override def iterator = {
      if (value != 0.0) {
        super.iterator
      } else {
        new java.util.Iterator[MTJMatrixEntry] {
          override def hasNext = false
          override def next = throw new UnsupportedOperationException
          override def remove = throw new UnsupportedOperationException
        }
      }
    }
  }

  /** The given vector as a column matrix */
  case class ColumnMatrix(vector : MTJVector) extends MTJAbstractMatrix(vector.size,1) {
    override def get(row : Int, col : Int) : Double = {
      check(row,col)
      return vector.get(row)
    }
    override def set(row : Int, col : Int, value : Double) : Unit = {
      check(row,col)
      vector.set(row, value)
    }
    override def iterator = {
      val iter = vector.iterator
      val entry = new MTJMatrixEntry {
        var inner : MTJVectorEntry = null
        override def get = inner.get
        override def row = inner.index
        override def column = 0
        override def set(value : Double) = inner.set(value)
      }
      new java.util.Iterator[MTJMatrixEntry] {
        override def hasNext = iter.hasNext
        override def next = { entry.inner = iter.next; entry; }
        override def remove = throw new UnsupportedOperationException()
      }
    }
  }
  
  /** A matrix pretending to be the transpose of the given matrix */
  case class TransposeMatrix(matrix : MTJMatrix) extends MTJAbstractMatrix(matrix.numColumns, matrix.numRows) {
    override def get(row : Int, col : Int) : Double =
      return matrix.get(col,row);
    
    override def set(row : Int, col : Int, value : Double) : Unit =
      matrix.set(col,row,value);
    
    override def iterator = {
      val iter = matrix.iterator
      val entry = new MTJMatrixEntry {
        var inner : MTJMatrixEntry = null
        override def get = inner.get
        override def row = inner.column
        override def column = inner.row
        override def set(value : Double) = inner.set(value)
      }
      new java.util.Iterator[MTJMatrixEntry] {
        override def hasNext = iter.hasNext
        override def next = { entry.inner = iter.next; entry; }
        override def remove = throw new UnsupportedOperationException()
      }
    }
  }
  
  /** A matrix like the given matrix, but pretending to start at rowOffset,colOffset */
  case class OffsetMatrix(matrix : MTJMatrix, rowOffset : Int, colOffset : Int) extends MTJAbstractMatrix(matrix.numRows + rowOffset, matrix.numColumns + colOffset) {
    override def get(row : Int, col : Int) : Double =
      return matrix.get(row - rowOffset, col - colOffset);
    
    override def set(row : Int, col : Int, value : Double) : Unit =
      matrix.set(row - rowOffset, col - colOffset, value);
    
    override def iterator = {
      val iter = matrix.iterator
      val entry = new MTJMatrixEntry {
        var inner : MTJMatrixEntry = null
        override def get = inner.get
        override def row = inner.row + rowOffset
        override def column = inner.column + colOffset
        override def set(value : Double) = inner.set(value)
      }
      new java.util.Iterator[MTJMatrixEntry] {
        override def hasNext = iter.hasNext
        override def next = { entry.inner = iter.next ; entry; }
        override def remove = throw new UnsupportedOperationException
      }
    }
  }
  
  /** Creates a block matrix from the given array of matrices ordered by row */
  case class BlockMatrix(blocks : Array[Array[MTJMatrix]]) extends
    MTJAbstractMatrix(blocks.map(_(0).numRows).reduceLeft(_+_), blocks(0).map(_.numColumns).reduceLeft(_+_)) {
    
    val numBlockRows = blocks.length
    val numBlockCols = blocks(0).length
      
    val rowOffsets =
      Array(0) ++ (for (row <- 1 to numBlockRows)
        yield blocks.subArray(0,row).map(_(0).numRows).reduceLeft(_+_));
    
    val colOffsets =
      Array(0) ++ (for (col <- 1 to numBlockCols)
        yield blocks(0).subArray(0,col).map(_.numColumns).reduceLeft(_+_));

    @inline def getBlockRow(row : Int) = {
      var i = 0;
      while (rowOffsets(i+1) < row) { i += 1; }
      i;
    }

    @inline def getBlockCol(col : Int) = {
      var i = 0;
      while (colOffsets(i+1) < col) { i += 1; }
      i;
    }
    
    // make sure everythings is good to go
    if (blocks.map(_.length).toList.removeDuplicates.length != 1) {
      throw new IllegalArgumentException("Must have same number of blocks in each row");
    }
    for (col <- 0 until numBlockCols) {
      // assert each column has same numColumns
      if (blocks.map(_(col).numColumns).toList.removeDuplicates.length != 1) {
        throw new IllegalArgumentException("Must have same number of columns in each column block "+col)
      }
    }
    for (row <- 0 until numBlockRows) {
      // assert each row has same numColumns
      if (blocks(row).map(_.numRows).toList.removeDuplicates.length != 1) {
        throw new IllegalArgumentException("Must have same number of rows in each row block "+row)
      }
    }
    
    override def get(row : Int, col : Int) : Double = {
      val blockRow = getBlockRow(row);
      val blockCol = getBlockCol(col);
      blocks(blockRow)(blockCol).get(row-rowOffsets(blockRow), col-colOffsets(blockCol));
    }
    
    override def set(row : Int, col : Int, value : Double) : Unit = {
      val blockRow = getBlockRow(row);
      val blockCol = getBlockCol(col);
      blocks(blockRow)(blockCol).set(row-rowOffsets(blockRow), col-colOffsets(blockCol), value);
    }
    
    override def iterator : java.util.Iterator[MTJMatrixEntry] = {
      def scalaIterator(iter : java.util.Iterator[MTJMatrixEntry]) =
        new Iterator[MTJMatrixEntry] {
          override def hasNext = iter.hasNext;
          override def next = iter.next;
        }
      
      val iterators : Iterable[OffsetMatrix] =
        for (blockRow <- 0 until numBlockRows;
             blockCol <- 0 until numBlockCols)
        yield new OffsetMatrix(blocks(blockRow)(blockCol),rowOffsets(blockRow),colOffsets(blockCol));
      val iterator = iterators.map{block => scalaIterator(block.iterator)}.reduceLeft(_++_);

      return new java.util.Iterator[MTJMatrixEntry] {
        override def hasNext = iterator.hasNext
        override def next = iterator.next
        override def remove = throw new UnsupportedOperationException();
      }
    }
  }
  
}
