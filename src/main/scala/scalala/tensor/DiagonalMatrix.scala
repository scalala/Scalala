package scalala
package tensor

import domain.TableDomain
import scalar.Scalar

/**
 * A matrix with values only on its diagonal, as defined by the
 * given vector.
 * 
 * @author dlwh, dramage
 */
class DiagonalMatrix[Vec,V](val diag : Vec)
(implicit val scalar: Scalar[V], view: Vec<:<Vector[V])
extends Matrix[V] with MatrixLike[V,DiagonalMatrix[Vec,V]] {
  override def numCols = diag.size;
  
  override def numRows = diag.size;
  
  override def nonzeroSize = diag.nonzeroSize;
  
  override def apply(i: Int, j: Int): V = {
    diag.checkKey(i);
    diag.checkKey(j);
    if (i == j) diag(i) else scalar.zero
  }
  
  override def foreachNonZeroKey[U](fn : (((Int,Int))=>U)) : Boolean = {
    diag.foreachNonZeroKey((i : Int) => fn((i,i)));
    false;
  }
    
  override def foreachNonZeroValue[U](fn : (V=>U)) : Boolean = {
    diag.foreachNonZeroValue(fn);
    false;
  }

  override def foreachNonZeroPair[U](fn : ((Int,Int),V)=>U) : Boolean = {
    diag.foreachNonZeroPair((i : Int, v : V) => fn((i,i),v));
    false;
  }
  
  override def foreachNonZeroTriple[U](fn : (Int,Int,V)=>U) : Boolean =
    diag.foreachNonZeroPair((i : Int, v : V) => fn(i,i,v));
    
  override def keysIteratorNonZero =
    diag.keysIteratorNonZero.map(i => (i,i));
  
  override def valuesIteratorNonZero =
    diag.valuesIteratorNonZero;
  
  override def pairsIteratorNonZero =
    diag.pairsIteratorNonZero.map(tup => ((tup._1,tup._1),tup._2));
  
  override def triplesIteratorNonZero =
    diag.pairsIteratorNonZero.map(tup => (tup._1, tup._1, tup._2));
}

