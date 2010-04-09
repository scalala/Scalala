package scalala.tensor.dense

import scalala.collection.MergeableSet
import scalala.tensor.Tensor;

final class DenseSeqTensor(val dims: Seq[Int], data: Array[Double])
  extends DoubleArrayData(data) with Tensor[Seq[Int]] with DenseTensor[Seq[Int]] {

  def this(dims: Seq[Int]) = this(dims,new Array(dims.reduceLeft(_ * _)));

  require(dims.reduceLeft(_ * _) == data.size);

  @inline final def index(indices: Seq[Int]) : Int = {
    var i = 0;
    var index = 0;
    require(indices.size == dims.size);
    while(i < indices.size) {
      require(indices(i) >= 0 && indices(i) < dims(i));
      index = indices(i) + dims(i) * index;
    }
    index;
  }

  override def copy = {
    val arr = new Array[Double](size);
    System.arraycopy(data,0,arr,0,size);
    new DenseSeqTensor(dims, arr);
  }

  def domain = new MergeableSet[Seq[Int]] {
    override lazy val size = dims.reduceLeft(_ * _);
    def iterator = {
      def unroll(seq: Seq[Int]) : Iterator[Seq[Int]]  = {
        if(seq.isEmpty) Iterator.empty
        else {
          val rest = unroll(seq.tail);
          for { s <- rest; i <- 0 until seq(0) iterator } yield i +: s;
        }
      }
      unroll(dims);
    }

    override def contains(ind: Seq[Int]) = {
      ind.length == dims.length && (0 until ind.length).forall { i => ind(i) == dims(i) }
    }

  }

  def apply(indices: Seq[Int]) = data(index(indices));
  def update(indices: Seq[Int], v: Double) = { data(index(indices)) = v; }

  def like = new DenseSeqTensor(dims);
}
