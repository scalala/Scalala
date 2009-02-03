package scalala.library

/**
 * Where a function shares its name with a Matlab function, the
 * behavior should be more or less consist with its Matlab
 * counterpart.
 * 
 * The primary difference is that some commands that normally return an
 * n x n square matrix now return a column vector of size n. e.g.
 * ones(n) here is a vector that would be ones(n,1) in matlab.  The
 * main reason for the distinction is that Vector and Matrix are
 * different types in MTJ, so ones(n,1) returns a Matrix of size n by 1
 * which shouldn't need its own conversion back to a vector.
 */
trait Library {
  def DenseMatrix(rows : Int, cols : Int) : Matrix;
  
  def DenseVector(size : Int) : Vector;
  
  def DenseVector(values : Array[Double]) : Vector;
  
  def DenseVector(values : Double*) : Vector;
  
  def DenseVector(values : Vector) : Vector;
  
  def SparseVector(size : Int) : Vector;
  
  def DiagonalMatrix(diagonal : Vector) : Matrix = {
    new Matrix {
      override def rows = diagonal.size;
      override def cols = diagonal.size;
      override def get(row : Int, col : Int) = 
        if (row == col) diagonal.get(row) else 0.0;
      override def set(row : Int, col : Int, value : Double) = {
        if (row == col) {
          diagonal.set(row,value)
        } else {
          throw new UnsupportedOperationException();
        }
      }
      override def copy = DiagonalMatrix(diagonal.copy).asInstanceOf[this.type];
    }
  }
  
  def ScalarMatrix(value : Double, rows : Int, cols : Int) : Matrix = {
    val _rows = rows;
    val _cols = cols;
    new Matrix {
      override def rows = _rows;
      override def cols = _cols;
      override def get(row : Int, col : Int) = value;
      override def set(row : Int, col : Int, value : Double) =
        throw new UnsupportedOperationException();
      override def copy = this;
    }
  }
  
  def ColMatrix(vector : Vector) : Matrix = {
    new Matrix {
      override def rows = vector.size;
      override def cols = 1;
      override def get(row : Int, col : Int) = {
        check(row,col);
        vector.get(row);
      }
      override def set(row : Int, col : Int, value : Double) = {
        check(row,col);
        vector.set(row,value);
      }
      override def copy = ColMatrix(vector.copy).asInstanceOf[this.type];
    }
  }
  
  def RowMatrix(vector : Vector) : Matrix = {
    new Matrix {
      def rows = 1;
      def cols = vector.size;
      override def get(row : Int, col : Int) = {
        check(row,col);
        vector.get(col);
      }
      override def set(row : Int, col : Int, value : Double) = {
        check(row,col);
        vector.set(col,value);
      }
      override def copy = RowMatrix(vector.copy).asInstanceOf[this.type];
    }
  }
  
  def TransposeMatrix(matrix : Matrix) : Matrix = {
    new Matrix {
      def rows = matrix.cols;
      def cols = matrix.rows;
      override def get(row : Int, col : Int) = matrix.get(col,row);
      override def set(row : Int, col : Int, value : Double) = matrix.set(col,row,value);
      override def copy = TransposeMatrix(matrix.copy).asInstanceOf[this.type];
    }
  }
  
  def BlockMatrix(blocks : Seq[Seq[Matrix]]) : Matrix = {
    throw new UnsupportedOperationException();
  }
  
  //
  // basic scala ops from Math.
  //
  
  /** Log a numeric value */
  @inline def log(v : Double) : Double = Math.log(v);
  
  val NaN = java.lang.Double.NaN;
  
  @inline def isnan(a : Double) : Boolean = java.lang.Double.isNaN(a);

  /** Alias for Math.sqrt. */
  @inline def sqrt(x : Double) = Math.sqrt(x);
}
