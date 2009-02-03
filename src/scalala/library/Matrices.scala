package scalala.library

trait Matrices extends Library with Vectors {
  /** A matrix of size m by n with 1 everywhere */
  def ones(rows : Int, cols : Int) : Matrix = {
    val m = DenseMatrix(rows,cols);
    for (i <- 0 until rows; j <- 0 until cols) m.set(i,j,1.0);
    return m;
  }
  
  /** A matrix of size m by n with 0 everywhere */
  def zeros(rows : Int, cols : Int) : Matrix = DenseMatrix(rows,cols);
  
  /** Sums the columns of the given matrix, returning a row vector */
  def sum(m : Matrix) : Matrix = sum(m, 1);
  
  /** Sums along the given dimension of the matrix (1 for rows, 2 for cols) */
  def sum(m : Matrix, dim : Int) : Matrix = {
    dim match {
      case 1 => {
        val sum = DenseMatrix(1, m.cols);
        for (entry <- m.elements) {
          sum(0, entry.col) += entry.get;
        }
        sum;
      }
      case 2 => {
        val sum = DenseMatrix(m.rows, 1);
        for (entry <- m.elements) {
          sum(entry.row, 0) += entry.get;
        }
        sum;
      }
      case _ => {
        throw new IndexOutOfBoundsException
      }
    }
  }
  
  /** The maximum element of a matrix. */
  def max(m : Matrix) : Double =
    max(vec(m));
  
  /** The minimum element of a matrix. */
  def min(m : Matrix) : Double =
    min(vec(m));
  
  
  //
  // Data formatters, mungers, etc
  //
  
  def vec(m : Matrix) : Vector = {
    val v = DenseVector(m.rows * m.cols);
    var i = 0;
    for (e <- m.elements) {
      v(i) = e.get;
      i += 1;
    }
    v;
  }
    
  /** Returns a square diagonal matrix of the given size */
  def diag(n : Int) : Matrix = {
    return diag(ones(n));
  }
  
  /**
   * Returns a diagonal matrix with the given vector on the diagonal.
   * Copies the contents of the underlying matrix.
   */
  def diag(v : Vector) : Matrix =
    DiagonalMatrix(v.copy);
  
  /**
   * Turns the given matrices into a block diagonal matrix.
   */
  def blkdiag(blocks : Seq[Matrix]) : Matrix = {
    def zeros() : Array[Matrix] =
      blocks.map(m => ScalarMatrix(0.0, m.rows, m.cols)).toArray
      
    def row(pos : Int) = {
      val row = zeros();
      row(pos) = blocks(pos);
      row
    }
    
    BlockMatrix((0 until blocks.length).map(row).toArray)
  }
}
