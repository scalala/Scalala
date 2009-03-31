package scalala.tensor.dense;

import scalala.tensor.Vector;
import scalala.collection.domain.{Domain, IntSpanDomain};

import scalala.tensor.Tensor.CreateException;

import scalala.tensor.spans.IntSpans._;

/**
 * A vector backed by a dense array of doubles.
 * 
 * @author dramage
 */
class DenseVector(data : Array[Double]) extends DoubleArrayData(data) with Vector {
  
  /** Cannot change default value for dense tensors. */
  override def default_=(update : Double) = {};
  
  override def size = data.size;
  
  override def apply(i : Int) = data(i);
  override def update(i : Int, value : Double) = data(i) = value;
  
  override val activeDomain : Set[Int] = 0 until size;
  
  override def create[J](domain : Domain[J]) : Tensor[J] = domain match {
    case IntSpanDomain(0,len) => new DenseVector(new Array[Double](len));
    case _ => throw new CreateException("Cannot create DenseVector with domain "+domain);
  }
  
  override def copy = new DenseVector(data.toArray).asInstanceOf[DenseVector.this.type];
  
  override def zero() = {
    this.default = 0;
    java.util.Arrays.fill(data, 0.0);
  }
}
