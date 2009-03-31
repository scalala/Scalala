package scalala.tensor;

import scalala.collection.domain.{Domain1, IntSpanDomain};

/**
 * A standard numerical Tensor1 defined over 0 inclusive to
 * size exclusive.
 * 
 * @author dramage
 */
trait Vector extends Tensor1[Int] {
  def size : Int;
  
  final override val domain1 : Domain1[Int] = IntSpanDomain(0, size);
}

object Vector {
  def apply(values : Double*) : Vector =
    new dense.DenseVector(values.toArray);
}
