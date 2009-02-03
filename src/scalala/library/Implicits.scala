package scalala.library

import scalala.ScalalaValueException;

trait Implicits extends Library {
  
  //
  // Type promotions
  //

  implicit def iDenseMatrixFromSeqSeq[T<:AnyVal](data : Seq[Seq[T]]) : Matrix = {
    val numRows = data.length
    val numCols = data map (_.length) reduceLeft Math.max
    val matrix  = DenseMatrix(numRows, numCols)
    for (i <- 0 until data.length) {
      val seq = data(i)
      if (seq.length >= 1) {
             if (seq(0).isInstanceOf[Double]) { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Double]; }
        else if (seq(0).isInstanceOf[Float])  { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Float]; }
        else if (seq(0).isInstanceOf[Int])    { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Int]; }
        else if (seq(0).isInstanceOf[Long])   { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Long]; }
        else if (seq(0).isInstanceOf[Short])  { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Short]; }
        else if (seq(0).isInstanceOf[Byte])   { for (j <- 0 until seq.length) matrix(i,j) = seq(j).asInstanceOf[Byte]; }
        else throw new ScalalaValueException("Unrecognized numeric type in sequence promotion");
      }
    }
    return matrix
  }
  
  implicit def iDenseVectorFromSeq[T<:AnyVal](seq : Seq[T]) : Vector = {
    val v = DenseVector(seq.length);
    if (seq.length >= 1) {
           if (seq(0).isInstanceOf[Double]) { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Double]; }
      else if (seq(0).isInstanceOf[Float])  { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Float]; }
      else if (seq(0).isInstanceOf[Int])    { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Int]; }
      else if (seq(0).isInstanceOf[Long])   { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Long]; }
      else if (seq(0).isInstanceOf[Short])  { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Short]; }
      else if (seq(0).isInstanceOf[Byte])   { for (i <- 0 until seq.length) v(i) = seq(i).asInstanceOf[Byte]; }
      else throw new ScalalaValueException("Unrecognized numeric type in sequence promotion");
    }
    return v;
  }

}
