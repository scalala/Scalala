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

import scalala.ScalalaValues._

//
// Value types for linear algebra, including matrices, vectors, scalars.
//

/**
 * A TensorValue is multi-dimensional array value.  If E is Int, then
 * the tensor is naturally considered a Vector.  If it is an (Int,Int),
 * then a Matrix.
 */
sealed trait Tensor[I,E<:TensorEntry[I]] extends PartialFunction[I,Double] with Iterable[E] {
  /** Size of the tensor */
  def size : I
  
  /** Gets the value at the given index */
  def get(index : I) = apply(index)
  
  /** Sets the value at the given index */
  def set(index : I, value : Double) : Unit = update(index, value)
  
  /** Updates the value at the given index */
  def update(index : I, value : Double) : Unit
  
  override def toString : String =
    elements.mkString("\n")
}

/**
 * A (mutating) entry for iterating the non-zero elements of the tensor
 */
sealed trait TensorEntry[I] {
  def index : I
  def get : Double
  def set(value : Double) : Unit
  
  override def toString : String =
    index + " " + get;
}

/** Scalars are un-indexed Tensors. */
trait Scalar extends Tensor[Unit,ScalarEntry] {}
trait ScalarEntry extends TensorEntry[Unit] {}

/** Vectors are Tensors indexed by a single Int. */
trait Vector extends Tensor[(Int),VectorEntry] {
  //
  // Tensor[(Int,Int)] implementation
  //
  
  @inline override def apply(index : Int) = get(index);
  @inline override def update(index : Int, value : Double) = set(index, value);
  @inline override def isDefinedAt(index : Int) =
    index < size && index >= 0
}

trait VectorEntry extends TensorEntry[(Int)] {}

/** Matrices are Tensors indexed by a pair of Ints. */
trait Matrix extends Tensor[(Int,Int),MatrixEntry] {
  //
  // pure abstract methods
  //
  
  /** Number of rows in the matrix. */
  def rows : Int
  
  /** Number of columns in the matrix. */
  def cols : Int
  
  /** Returns the value at the given row, col index. */
  def get(row : Int, col : Int) : Double

  /** Sets the given element to the given value. */
  def set(row : Int, col : Int, value : Double) : Unit
  
  /**
   * Returns all (but not necessarily only) non-zero elements in
   * arbitrary order.
   * 
   * Implementations are encouraged to mutate and return a single
   * MatrixValueEntry between calls to next.
   */
  def elements : Iterator[MatrixEntry] = elementsByRow
  
  //
  // accessors
  //
  
  def apply(row : Int, col : Int) = get(row, col)
  
  /** For matrix((_,1)) and matrix((0,_)) */
  def apply(select : (Int=>(Int,Int))) : Matrix = {
    // find whether row or column
    select(-1) match {
    case (-1,-1)  => throw new IllegalArgumentException("Index of out range")
    case (-1,col) => {ColMatrix((0 until rows).map(row => get(row,col)).toList)}
    case (row,-1) => {RowMatrix((0 until cols).map(col => get(row,col)).toList)}
    case _        => throw new IllegalArgumentException("Invalid index selector")
    }
  }
  
  def check(row : Int, col : Int) = {
    if (row < 0 || col < 0) {
      throw new IndexOutOfBoundsException("Matrix indeces must be non-negative");
    }
    if (row > rows) {
      throw new IndexOutOfBoundsException("Row "+row+" is out of range");
    }
    if (col > cols) {
      throw new IndexOutOfBoundsException("Col "+col+" is out of range");
    }
  }
  
  //
  // Tensor[(Int,Int)] implementation
  //
  
  @inline override def size = (rows,cols);
  
  @inline override def apply(index : (Int,Int)) =
    get(index._1, index._2);
  
  @inline override def update(index : (Int,Int), value : Double) =
    set(index._1, index._2, value);
  
  @inline override def isDefinedAt(index : (Int,Int)) =
    index._1 < rows && index._2 < cols && index._1 >= 0 && index._2 >= 0
  
  @inline def update(row : Int, col : Int, value : Double) =
    set(row, col, value);
  
  //
  // iterators and views -- to be overridden by subclasses for speed
  //
  
  /**
   * Returns all non-zero elements, incrementally by row first then by column
   * within row.
   * 
   * Implementations may mutate and return a single MatrixValueEntry between
   * calls to next.  The default implementation uses the elements iterator,
   * but is very inefficient in time and space.
   */
  def elementsByRow : Iterator[MatrixEntry] = {
    // elementsSorted((a:(Int,Int,Double),b:(Int,Int,Double)) => if (a._1 < b._1 ) true else (a._2 < b._2))
    var _row = 0;
    var _col = 0;
    
    val entry = new MatrixEntry {
      override def row = _row;
      override def col = _col;
      override def get = Matrix.this.get(_row,_col);
      override def set(value:Double) = Matrix.this.set(_row,_col,value);
    }
    
    return new Iterator[MatrixEntry] {
      var _next_row = 0;
      var _next_col = 0;
      
      override def hasNext : Boolean = {
        _next_row = _row;
        _next_col = _col;
        while (_next_row < rows) {
          if (_next_col == cols) {
            _next_col = 0;
          }
          while (_next_col < cols) {
            if (get(_next_row,_next_col) != 0.0) {
              return true;
            }
            _next_col += 1;
          }
          _next_row += 1;
        }
        return false;
      }
    
      override def next : MatrixEntry = {
        _row = _next_row;
        _col = _next_col;
        return entry;
      }
    }
  }
  
  /**
   * Returns all non-zero elements, incrementally by column first then by row
   * within column.
   * 
   * Implementations may mutate and return a single MatrixValueEntry between
   * calls to next.  The default implementation uses the elements iterator,
   * but is very inefficient in time and space.
   */
  def elementsByCol : Iterator[MatrixEntry] = {
    // elementsSorted((a:(Int,Int,Double),b:(Int,Int,Double)) => if (a._2 < b._2 ) true else (a._1 < b._1))
    var _row = 0;
    var _col = 0;
    
    val entry = new MatrixEntry {
      override def row = _row;
      override def col = _col;
      override def get = Matrix.this.get(_row,_col);
      override def set(value:Double) = Matrix.this.set(_row,_col,value);
    }
    
    return new Iterator[MatrixEntry] {
      var _next_row = 0;
      var _next_col = 0;
      
      override def hasNext : Boolean = {
        _next_row = _row;
        _next_col = _col;
        while (_next_col < cols) {
          if (_next_row == rows) {
            _next_row = 0;
          }
          while (_next_row < rows) {
            if (get(_next_row,_next_col) != 0.0) {
              return true;
            }
            _next_row += 1;
          }
          _next_col += 1;
        }
        return false;
      }
    
      override def next : MatrixEntry = {
        _row = _next_row;
        _col = _next_col;
        return entry;
      }
    }
  }
    
  /** To get contents as a map */
  def asMap : scala.collection.Map[(Int,Int),Double] = {
    return new scala.collection.Map[(Int,Int),Double] {
      override def size = rows * cols;
      
      override def get(key : (Int,Int)) = Some(Matrix.this.get(key._1,key._2));
      
      override def elements = new Iterator[((Int,Int),Double)] {
        val iter = Matrix.this.elements;
        override def hasNext = iter.hasNext;
        
        override def next = {
          val e = iter.next
          ((e.row,e.col),e.get)
        }
      }
    }
  }
  
  private def elementsSorted(comparator : (((Int,Int,Double),(Int,Int,Double)) => Boolean)) : Iterator[MatrixEntry] = {
    val tups = for (entry <- elements) yield (entry.row, entry.col, entry.get);
    val iter = tups.toList.sort(comparator).elements;
    var _entry : (Int,Int,Double) = null;
    
    val entry = new MatrixEntry {
      override def index = (_entry._1, _entry._2)
      override def row = _entry._1;
      override def col = _entry._2;
      override def get = _entry._3;
      override def set(x : Double) =
        Matrix.this.set(row,col,x);
    }
    
    new Iterator[MatrixEntry] {
      override def hasNext = iter.hasNext;
      override def next = {
        _entry = iter.next;
        entry;
      }
    }
  }
  
  override def toString : String = {
    if (cols <= 10) {
      val r = for (i <- 0 until rows)
        yield ((0 until cols).map(j => String.format("%8.4f",double2Double(get(i,j)))).mkString(" "));
      r.mkString("\n")
    } else {
      super.toString
    }
  }
}

trait MatrixEntry extends TensorEntry[(Int,Int)]{
  def row : Int
  def col : Int
  override def index : (Int,Int) = (row,col)
}


object ScalalaValues {
  import ScalalaMTJ._
  
  //
  // Lightweight wrappers for matrices backed by scalars or vectors
  //

  /** The given scalar as an immutable matrix of the given size */
  case class ScalarMatrix(value : Double, val rows : Int, val cols : Int) extends Matrix {
    override def get(row : Int, col : Int) : Double = {
      check(row,col);
      return value;
    }
    override def set(row : Int, col : Int, value : Double) = {
      throw new UnsupportedOperationException("Cannot change values in ScalarMatrix");
    } 
  }
  
  /** The given vector as a row matrix */
  case class RowMatrix(val vector : Vector) extends Matrix {
    override def rows = 1;
    override def cols = vector.size;
    
    override def get(row : Int, col : Int) : Double = {
      check(row,col);
      return vector.get(col);
    }
    
    override def set(row : Int, col : Int, value : Double) : Unit = {
      check(row,col);
      vector.set(col, value);
    }
    
    override def elements : Iterator[MatrixEntry] = {
      val iter = vector.elements
      val entry = new MatrixEntry {
        var inner : VectorEntry = null
        override def get = inner.get
        override def row = 0
        override def col = inner.index
        override def set(value : Double) = inner.set(value)
      }
      new Iterator[MatrixEntry] {
        override def hasNext = iter.hasNext
        override def next = { entry.inner = iter.next; entry; }
      }
    }
  }
  
  /** The given vector as a column matrix */
  case class ColMatrix(val vector : Vector) extends Matrix {
    override def rows = vector.size;
    override def cols = 1;
    
    override def get(row : Int, col : Int) : Double = {
      check(row,col);
      return vector.get(row);
    }
    
    override def set(row : Int, col : Int, value : Double) : Unit = {
      check(row,col);
      vector.set(row, value);
    }
    
    override def elements : Iterator[MatrixEntry] = {
      val iter = vector.elements
      val entry = new MatrixEntry {
        var inner : VectorEntry = null
        override def get = inner.get
        override def row = inner.index
        override def col = 0
        override def set(value : Double) = inner.set(value)
      }
      new Iterator[MatrixEntry] {
        override def hasNext = iter.hasNext
        override def next = { entry.inner = iter.next; entry; }
      }
    }
  }
  
  /** A matrix pretending to be the transpose of the given matrix */
  case class TransposeMatrix(matrix : Matrix) extends Matrix {
    override def rows = matrix.cols;
    override def cols = matrix.rows;
    
    override def get(row : Int, col : Int) : Double =
      return matrix.get(col,row);
    
    override def set(row : Int, col : Int, value : Double) : Unit =
      matrix.set(col,row,value);
    
    override def elements : Iterator[MatrixEntry] = {
      val iter = matrix.elements
      val entry = new MatrixEntry {
        var inner : MatrixEntry = null
        override def get = inner.get
        override def row = inner.col
        override def col = inner.row
        override def set(value : Double) = inner.set(value)
      }
      new Iterator[MatrixEntry] {
        override def hasNext = iter.hasNext
        override def next = { entry.inner = iter.next; entry; }
      }
    }
  }
  
  //
  // Type promotions
  //
  // TODO: fix type promotions to matrices from sequences using same trick as vector
  //
  
  implicit def iDenseMatrixFromInts[E<:Seq[Int]](data : Seq[E]) : Matrix = {
    val numRows = data.length
    val numCols = data map (_.length) reduceLeft Math.max
    val matrix  = DenseMatrix(numRows, numCols)
    for (i <- 0 until data.length) {
      val row = data(i)
      for (j <- 0 until row.length) {
        matrix.set(i,j,row(j))
      }
    }
    return matrix
  }
  
  implicit def iDenseMatrixFromDoubles[E<:Seq[Double]](data : Seq[E]) : Matrix = {
    val numRows = data.length
    val numCols = data map (_.length) reduceLeft Math.max
    val matrix  = DenseMatrix(numRows, numCols)
    for (i <- 0 until data.length) {
      val row = data(i)
      for (j <- 0 until row.length) {
        matrix.set(i,j,row(j))
      }
    }
    return matrix
  }
  
  implicit def iDenseVectorFromArray(data : Array[Double]) : Vector =
    DenseVector(data);
  
  implicit def iDenseVectorFromArray(data : Array[Float]) : Vector =
    DenseVector(Array.fromFunction((i:Int) => data(i).asInstanceOf[Double])(data.size));
  
  implicit def iDenseVectorFromArray(data : Array[Int]) : Vector =
    DenseVector(Array.fromFunction((i:Int) => data(i).asInstanceOf[Double])(data.size));
  
  implicit def iDenseVectorFromArray(data : Array[Long]) : Vector =
    DenseVector(Array.fromFunction((i:Int) => data(i).asInstanceOf[Double])(data.size));
  
  implicit def iDenseVectorFromArray(data : Array[Short]) : Vector =
    DenseVector(Array.fromFunction((i:Int) => data(i).asInstanceOf[Double])(data.size));
  
  implicit def iDenseVectorFromArray(data : Array[Byte]) : Vector =
    DenseVector(Array.fromFunction((i:Int) => data(i).asInstanceOf[Double])(data.size));
  
  implicit def iDenseVectorFromIterable[T<:AnyVal](iter : Iterable[T]) : Vector = {
    iDenseVectorFromSeq(iter.toList);
  }
  
  implicit def iDenseVectorFromSeq[T<:AnyVal](seq : Seq[T]) : Vector = {
    val v = DenseVector(seq.length);
    
    if (seq.length >= 1) {
      seq(0) match {
        case (x:Double) => for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Double];
        case (x:Float)  => for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Float];
        case (x:Int)    => for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Int];
        case (x:Long)   => for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Long];
        case (x:Short)  => for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Short];
        case (x:Byte)   => for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Byte];
      }
    }
    
    return v;
  }
  
}
