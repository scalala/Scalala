package scalala.tensor;

import scalala.collection.domain.{Domain2, IntSpanDomain};

/**
 * A standard numerical Tensor2 defined over (0,0) inclusive to 
 * (rows,cols) exclusive.
 * 
 * @author dramage
 */
trait Matrix extends Tensor2[Int,Int] {
  def rows : Int;
  def cols : Int;
  
  @inline final protected def check(row : Int, col : Int) = {
    if (row < 0 || row >= rows || col < 0 || col >= cols) {
      throw new Predef.IndexOutOfBoundsException(
        "Matrix out of bounds: "+(row,col)+" size was "+size)
    }
  }
  
  @inline final def size = (rows, cols);
  
  final override val domain2 : Domain2[Int,Int] =
    Domain2(IntSpanDomain(0, rows), IntSpanDomain(0, cols));
  
  /*
  def apply(select : (Int => (Int,Int))) : Matrix = {
    select(-1) match {
      case (-1,-1)  => throw new IllegalArgumentException("Index of out range");
      case (-1,col) => this.select((0,col),(rows,col));
      case (row,-1) => this.select((row,0),(row,cols));
      case _        => throw new IllegalArgumentException("Invalid index selector");
    }
  }
  */
  
  /**
   * Selects the given submatrix.  Writes are passed through.
   */
  /*
  def select(start : (Int,Int), end : (Int,Int)) = new Matrix.Projection(this) {
    override def rows = end._1 - start._1;
    override def cols = end._2 - start._2;
    
    @inline final def transformRow(row : Int) = row + start._1;
    @inline final def transformCol(col : Int) = col + start._2;
    
    override def update(row : Int, col : Int, value : Double) =
      inner.update(transformRow(row), transformCol(col), value);
    
    override def apply(row : Int, col : Int) = 
      inner.apply(transformRow(row), transformCol(col));
    
    override def copy = {
      // TODO: implement
      throw new UnsupportedOperationException
    }
    
    override val activeDomain : Set[(Int,Int)] = {
      // TODO: make more efficient for sparse matrices
      // inner.activeDomain intersect (start until end);
      (0,0) until (rows,cols);
    }
  }
  */
}
