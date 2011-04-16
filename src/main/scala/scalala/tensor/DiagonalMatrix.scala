package scalala
package tensor

import domain.TableDomain
import scalar.Scalar

/**
 * 
 * @author dlwh
 */
class DiagonalMatrix[Vec,S](val innerVector: Vec)(implicit val scalar: Scalar[S], view: Vec<:<Vector[S])
  extends Matrix[S] with MatrixLike[S,DiagonalMatrix[Vec,S]] {
  def numCols = innerVector.size;
  def numRows = innerVector.size;
  def apply(i: Int, j: Int): S = {
    innerVector.checkKey(i);
    innerVector.checkKey(j);
    if(i == j) innerVector(i) else scalar.zero
  }

}