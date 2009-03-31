package scalala.tensor.operators;

import scalala.collection.domain.{Domain, Domain1, Domain2, DomainException};
import scalala.tensor.{Tensor, Tensor1, Tensor2};

trait OperatorSupport {
  /** Implicitly promotes a Tensor to a TensorIdentity operation. */
  implicit def iTensorOp[I](t : Tensor[I]) =
    TensorIdentity(t);
  
  /** Implicitly promotes a Tensor1 to a VectorOp */
  implicit def iVectorOp[I](t : Tensor1[I]) =
    VectorIdentity(t);
  
  /** Implicitly promotes a Tensor2 to a MatrixOp */
  implicit def iMatrixOp[I,J](t : Tensor2[I,J]) =
    MatrixIdentity(t);
  
  /** Implicits promotes a TensorOp to its Tensor value. */
  implicit def iTensor[I](op : TensorOp[I]) : Tensor[I] = op.value; 
}

object OperatorSupport extends OperatorSupport { }


  /**
   * Operations applicable to any tensor: updates by a scalar and
   * pairwise arithmetic.
   */
  abstract case class TensorOp[I]() {
    /** Returns the domain of the output of this Tensor. */
    def domain : Domain[I];
    
    /**
     * Returns the value of this operator in a Vector that might
     * be visible outside of the evaluator, i.e. that its value
     * should not be mutated.
     */
    def value : Tensor[I];

    /**
     * Returns the value of this operator in a vector than can safely
     * be over-written.
     */
    def working : Tensor[I] = value;
    
    // scalar operators
    def  + (s : Double) = TensorPlusScalar(this, s);
    def  - (s : Double) = TensorMinusScalar(this, s);
    def  * (s : Double) = TensorMultScalar(this, s);
    def  / (s : Double) = TensorDivScalar(this, s);
    def  ^ (s : Double) = TensorPowScalar(this, s);
    // def  < (s : Double) = TensorLTScalar(this, s);
    // def  > (s : Double) = TensorGTScalar(this, s);
    // def <= (s : Double) = TensorLTEScalar(this, s);
    // def >= (s : Double) = TensorGTEScalar(this, s);
    // def && (s : Double) = TensorAndTensor(this, s);
    // def || (s : Double) = TensorOrTensor(this, s);
    
    // tensor operators
    def :+ (op : TensorOp[I]) = TensorPlusTensor(this,op);
    def :- (op : TensorOp[I]) = TensorMinusTensor(this,op);
    def :* (op : TensorOp[I]) = TensorMultTensor(this,op);
    def :/ (op : TensorOp[I]) = TensorDivTensor(this,op);
    
    /** Fixed alias for :+ */
    final def + (op : TensorOp[I]) = this :+ op;
    
    /** Fixed alias for :- */
    final def - (op : TensorOp[I]) = this :- op;
  }

  /** Extra operators for Tensor1 implementations as a column. */
  trait VectorOp[I] extends TensorOp[I] {
    final override def domain : Domain[I] = domain1;
    def domain1 : Domain1[I];
    
    final override def value : Tensor[I] = value1;
    def value1 : Tensor1[I];
    
    /** Transposes this matrix. */
    def t = VectorToRow(this);
    
    /** Vector-vector multiplication. */
    def * (op : RowVectorOp[I]) = (this.value1 dot op.value1);
  }

  /** Extra operators for Tensor1 implementations as a row. */
  trait RowVectorOp[I] extends TensorOp[I] {
    final override def domain : Domain[I] = domain1;
    def domain1 : Domain1[I];
    
    final override def value : Tensor[I] = value1;
    def value1 : Tensor1[I];
    
    /** Transposes this matrix. */
    def t = VectorToCol(this);
    
    /** Vector-matrix multiplication. */
    def *[J] (op : MatrixOp[I,J]) = VectorInnerMultMatrix(this, op);
    
    /** Vector-vector multiplication. */
    def * (op : VectorOp[I]) = (this.value1 dot op.value1);
  }
  
  /** Extra operators for Tensor2 implementations. */
  trait MatrixOp[I1,I2] extends TensorOp[(I1,I2)] {
    /** Fixed alias for domain2. */
    final override def domain : Domain[(I1,I2)] = domain2;
    
    /** Domain for this operation. */
    def domain2 : Domain2[I1,I2];
    
    /** Fixed alias for value2. */
    final override def value : Tensor[(I1,I2)] = value2;
    
    /** Value of this operation. */
    def value2 : Tensor2[I1,I2];
    
    /** Transposes this tensor. */
    def t = MatrixTranspose(this);
    
    /** Matrix-matrix multiplication. */
    def *[J] (op : MatrixOp[I2,J]) = MatrixInnerMultMatrix(this, op);
    
    /** Matrix-vector multiplication. */
    def * (op : VectorOp[I2]) = MatrixInnerMultVector(this, op);
  }
  
  /** An underlying tensor. */
  case class TensorIdentity[I](tensor : Tensor[I]) extends TensorOp[I] {
    override def domain = tensor.domain;
    override def value = tensor;
    override val working = tensor.copy;
  }
  
  /** An operator between a tensor and scalar. */
  abstract class TensorScalarOp[I](tensor : TensorOp[I], scalar : Double) extends TensorOp[I] {
    override def domain = tensor.domain;
  }
  
  /** An operator between two tensors defined on the same domain */
  abstract class TensorTensorOp[I](tensorA : TensorOp[I], tensorB : TensorOp[I]) extends TensorOp[I] {
    if (tensorA.domain != tensorB.domain) throw new Predef.IllegalArgumentException;
    override def domain = tensorA.domain;
  }
  
  case class TensorPlusScalar[I](tensor : TensorOp[I], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override lazy val value = {
      val rv = tensor.working;
      rv += scalar;
      rv;
    }
  }
  
  case class TensorMinusScalar[I](tensor : TensorOp[I], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override lazy val value = {
      val rv = tensor.working;
      rv -= scalar;
      rv;
    }
  }
  case class TensorMultScalar[I](tensor : TensorOp[I], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override lazy val value = {
      val rv = tensor.working;
      rv *= scalar;
      rv;
    }
  }
  
  case class TensorDivScalar[I](tensor : TensorOp[I], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override lazy val value = {
      val rv = tensor.working;
      rv /= scalar;
      rv;
    }
  }
  
  case class TensorPowScalar[I](tensor : TensorOp[I], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override lazy val value = {
      val rv = tensor.working;
      rv ^= scalar;
      rv;
    }
  }
  
  case class TensorPlusTensor[I](tensorA : TensorOp[I], tensorB : TensorOp[I]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :+= tensorB;
      rv;
    }
  }
  
  case class TensorMinusTensor[I](tensorA : TensorOp[I], tensorB : TensorOp[I]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :-= tensorB;
      rv;
    }
  }
  
  case class TensorMultTensor[I](tensorA : TensorOp[I], tensorB : TensorOp[I]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :*= tensorB;
      rv;
    }
  }
  
  case class TensorDivTensor[I](tensorA : TensorOp[I], tensorB : TensorOp[I]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :/= tensorB;
      rv;
    }
  }
  
  case class VectorIdentity[I](tensor1 : Tensor1[I]) extends TensorIdentity(tensor1) with VectorOp[I] {
    override def domain1 = tensor1.domain;
    override def value1 = tensor1;
  }
  
  case class MatrixIdentity[I,J](tensor2 : Tensor2[I,J]) extends TensorIdentity(tensor2) with MatrixOp[I,J] {
    override def domain2 = tensor2.domain;
    override def value2 = tensor2;
  }
  
  case class VectorToRow[I](op : VectorOp[I]) extends RowVectorOp[I] {
    override def domain1 = op.domain1;
    override def value1 = op.value1;
    override def working = op.working;
  }
  
  case class VectorToCol[I](op : RowVectorOp[I]) extends VectorOp[I] {
    override def domain1 = op.domain1;
    override def value1 = op.value1;
    override def working = op.working;
  }
  
  case class MatrixTranspose[B,A](op : MatrixOp[A,B]) extends MatrixOp[B,A] {
    override lazy val domain2 = op.domain2.transpose;
    override lazy val value2 = op.value2.transpose;
  }
  
  case class MatrixInnerMultMatrix[A1,INNER,B2](t1 : MatrixOp[A1,INNER], t2 : MatrixOp[INNER,B2]) extends MatrixOp[A1,B2] {
    if (t1.domain2._2 != t2.domain2._1) throw new Predef.IllegalArgumentException;
    override lazy val domain2 = Domain2(t1.domain2._1, t2.domain2._2);
    override lazy val value2 : Tensor2[A1,B2] = {
      val innerDomain = t1.domain2._2;
      val t1v = t1.value;
      val t2v = t2.value;
      val rv = t1v.create(domain2).asInstanceOf[Tensor2[A1,B2]];
      for (i <- domain2._1; j <- domain2._2) {
        var dot = 0.0;
        for (k <- innerDomain) {
          dot += t1v(i,k) * t2v(k,j);
        }
        rv(i,j) = dot;
      }
      rv;
    }
  }
 
  case class MatrixInnerMultVector[I,J](matrix : MatrixOp[I,J], vector : VectorOp[J]) extends VectorOp[I] {
    if (matrix.domain2._2 != vector.domain1) throw new DomainException;
    override def domain1 = matrix.domain2._1;
    override lazy val value1 : Tensor1[I] = {
      val mv = matrix.value;
      val vv = vector.value;
      val rv = vv.create(domain1).asInstanceOf[Tensor1[I]];
      for (i <- domain1) {
        var dot = 0.0;
        for (j <- matrix.domain2._2) {
          dot += mv(i,j) * vv(j);
        }
        rv(i) = dot;
      }
      rv;
    }
  }
  
  case class VectorInnerMultMatrix[I,J](vector : RowVectorOp[I], matrix : MatrixOp[I,J]) extends RowVectorOp[J] {
    if (vector.domain1 != matrix.domain2._1) throw new DomainException;
    override def domain1 = matrix.domain2._2;
    override lazy val value1 : Tensor1[J] = {
      val vv = vector.value;
      val mv = matrix.value;
      val rv = vv.create(domain1).asInstanceOf[Tensor1[J]];
      for (j <- domain1) {
        var dot = 0.0;
        for (i <- matrix.domain2._1) {
          dot += vv(i) * mv(i,j);
        }
        rv(j) = dot;
      }
      rv;
    }
  }
