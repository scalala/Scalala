package scalala;
package tensor;
package dense;

class DenseMatrixLinearizer(rows: Int, cols: Int) extends TensorLinearizer[(Int,Int),Int,DenseMatrix,DenseVector] {
  type ProjectedTensor = DenseVector;
  def linearize(t: DenseMatrix) = new DenseVector(t.data);
  def reshape(pt: DenseVector) = new DenseMatrix(rows, cols, pt.data);
}

class DenseSeqTensorLinearizer(dims: Seq[Int]) extends TensorLinearizer[Seq[Int], Int, DenseSeqTensor, DenseVector] {
  type ProjectedTensor = DenseVector;
  def linearize(t: DenseSeqTensor) = new DenseVector(t.data);
  def reshape(pt: DenseVector) = new DenseSeqTensor(dims, pt.data);
}