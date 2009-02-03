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

import scalala._;

trait MTJValues extends Library {
  import ScalalaMTJ._;
  
  def DenseVector(size : Int) : Vector =
    new no.uib.cipr.matrix.DenseVector(size);
  
  def DenseVector(values : Array[Double]) : Vector =
    new no.uib.cipr.matrix.DenseVector(values, false);
  
  def DenseVector(values : Double*) : Vector = {
    new no.uib.cipr.matrix.DenseVector(values.toArray, false);
  }
  
  def DenseVector(values : Vector) : Vector =
    new no.uib.cipr.matrix.DenseVector(values.toArray, false);
  
  def DenseMatrix(rows : Int, cols : Int) : Matrix =
    new no.uib.cipr.matrix.DenseMatrix(rows,cols);
  
  def SparseVector(size : Int) : Vector =
    new no.uib.cipr.matrix.sparse.SparseVector(size, Math.min(size/10,1000));
  
}

/**
 * MTJ compatibility wrappers for Scalala matrix types.
 */
object ScalalaMTJ {

  import scalala.{Vector,VectorEntry,Matrix,MatrixEntry}
  import no.uib.cipr.matrix.{Matrices => MTJMatrices}
  import no.uib.cipr.matrix.{Matrix => MTJMatrix,MatrixEntry => MTJMatrixEntry}
  import no.uib.cipr.matrix.{DenseMatrix => MTJDenseMatrix, AbstractMatrix => MTJAbstractMatrix}
  import no.uib.cipr.matrix.{Vector => MTJVector, VectorEntry => MTJVectorEntry, DenseVector => MTJDenseVector, AbstractVector => MTJAbstractVector}
  import no.uib.cipr.matrix.sparse.{SparseVector => MTJSparseVector,CompColMatrix => MTJCompColMatrix}
  
  //
  // Basic data type constructors
  //
  
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
    
    override def copy = new MTJMatrixWrapper(matrix.copy).asInstanceOf[this.type]
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
    
    override def copy = new MTJVectorWrapper(vector.copy).asInstanceOf[this.type]
  }
}  
