/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala.tensor.operators;

import scalala.collection.domain.{Domain, Domain1, Domain2, IntSpanDomain, DomainException};
import scalala.tensor.{Tensor, Tensor1, Tensor2};

/**
 * Implicit conversions between TensorOp's and Tensors.
 * 
 * @author dramage
 */
trait OperatorSupport {
  /** Implicitly promotes a Tensor to a TensorIdentity operation. */
  implicit def iTensorOp[I](t : Tensor[I]) : TensorOp[I,Tensor[I]] =
    TensorIdentity(t);
  
  /** Implicitly promotes a Tensor1 to a VectorOp */
  implicit def iTensor1Op[I](t : Tensor1[I]) : VectorOp[I,Tensor1[I]] =
    VectorIdentity(t);
  
  /** Implicitly promotes a Vector to a VectorOp */
  implicit def iVectorOp(t : Vector) : VectorOp[Int,Vector] =
    VectorIdentity(t);
  
  /** Implicitly promotes a Tensor2 to a MatrixOp */
  implicit def iTensor2Op[I,J](t : Tensor2[I,J]) : MatrixOp[I,J,Tensor2[I,J]] =
    MatrixIdentity(t);
  
  /** Implicitly promotes a Matrix to a MatrixOp */
  implicit def iMatrixOp(t : Matrix) : MatrixOp[Int,Int,Matrix] =
    MatrixIdentity(t);
  
  /** Implicitly promotes a TensorOp to its Tensor value. */
  implicit def iTensor[I,T<:Tensor[I]](op : TensorOp[I,T]) : T = op.value;
}

object OperatorSupport extends OperatorSupport { }


  /**
   * Operations applicable to any tensor: updates by a scalar and
   * pairwise arithmetic.
   */
  abstract case class TensorOp[I,T<:Tensor[I]]() {
    /** Returns the domain of the output of this Tensor. */
    def domain : Domain[I];
    
    /**
     * Returns the value of this operator in a Vector that might
     * be visible outside of the evaluator, i.e. that its value
     * should not be mutated.
     */
    def value : T;

    /**
     * Returns the value of this operator in a vector than can safely
     * be over-written.
     */
    def working : T = value;
    
    /** Creates a new tensor for the requested domain type. */
    def create[J](d : Domain[J]) : Tensor[J];
    
    // scalar operators
    def  + (s : Double) = TensorPlusScalar(this, s);
    def  - (s : Double) = TensorPlusScalar(this, -s);
    def  * (s : Double) = TensorMultScalar(this, s);
    def  / (s : Double) = TensorMultScalar(this, 1.0 / s);
    def  ^ (s : Double) = TensorPowScalar(this, s);
    def  < (s : Double) = TensorLTScalar(this, s);
    def  > (s : Double) = TensorGTScalar(this, s);
    def <= (s : Double) = TensorLTEScalar(this, s);
    def >= (s : Double) = TensorGTEScalar(this, s);
    def && (s : Double) = TensorAndScalar(this, s);
    def || (s : Double) = TensorOrScalar(this, s);
    
    /** Colon prefix is required to avoid conflation with .equals */
    final def :== (s : Double) = TensorEqScalar(this, s);
    
    /** Colon prefix is required to avoid conflation with .equals */
    final def :!= (s : Double) = TensorNeScalar(this, s);
    
    // tensor operators
    def :+  [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorPlusTensor(this,op);
    def :-  [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorMinusTensor(this,op);
    def :*  [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorMultTensor(this,op);
    def :/  [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorDivTensor(this,op);
    def :<  [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorLTTensor(this, op);
    def :>  [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorGTTensor(this, op);
    def :<= [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorLTETensor(this, op);
    def :>= [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorGTETensor(this, op);
    def :== [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorEqTensor(this, op);
    def :!= [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorNeTensor(this, op);
    def :&& [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorAndTensor(this, op);
    def :|| [T2<:Tensor[I]] (op : TensorOp[I,T2]) = TensorOrTensor(this, op);
    
    /** Fixed alias for :+ */
    final def +[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :+ op;
    /** Fixed alias for :- */
    final def -[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :- op;
    /** Fixed alias for :< */
    final def <[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :< op;
    /** Fixed alias for :> */
    final def >[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :> op;
    /** Fixed alias for :<= */
    final def <=[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :<= op;
    /** Fixed alias for :>= */
    final def >=[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :>= op;
    /** Fixed alias for :&& */
    final def &&[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :&& op;
    /** Fixed alias for :|| */
    final def ||[T2<:Tensor[I]] (op : TensorOp[I,T2]) = this :|| op;
  }

  /** Extra operators for Tensor1 implementations as a column. */
  trait VectorOp[I,T<:Tensor1[I]] extends TensorOp[I,T] {
    final override def domain = domain1;
    def domain1 : Domain1[I];
    
    final override def value = value1;
    def value1 : T;
    
    /** Transposes this matrix. */
    def t = VectorToRow(this);
    
    /** Vector-vector multiplication. */
    def *[T2<:Tensor1[I]] (op : RowVectorOp[I,T2]) = VectorOuterMultVector(this, op);
  }

  /** Extra operators for Tensor1 implementations as a row. */
  trait RowVectorOp[I,T<:Tensor1[I]] extends TensorOp[I,T] {
    final override def domain = domain1;
    def domain1 : Domain1[I];
    
    final override def value = value1;
    def value1 : T;
    
    /** Transposes this matrix. */
    def t = VectorToCol(this);
    
    /** Vector-matrix inner multiplication. */
    def *[J,T2<:Tensor2[I,J]] (op : MatrixOp[I,J,T2]) = VectorInnerMultMatrix(this, op);
    
    /** Vector-vector inner multiplication. */
    def *[T2<:Tensor1[I]] (op : VectorOp[I,T2]) = (this.value1 dot op.value1);
  }
  
  /** Extra operators for Tensor2 implementations. */
  trait MatrixOp[I1,I2,T<:Tensor2[I1,I2]] extends TensorOp[(I1,I2),T] {
    /** Fixed alias for domain2. */
    final override def domain = domain2;
    
    /** Domain for this operation. */
    def domain2 : Domain2[I1,I2];
    
    /** Fixed alias for value2. */
    final override def value = value2;
    
    /** Value of this operation. */
    def value2 : T;
    
    /** Transposes this tensor. */
    def t = MatrixTranspose(this);
    
    /** Matrix-matrix multiplication. */
    def *[J,T2<:Tensor2[I2,J]] (op : MatrixOp[I2,J,T2]) = MatrixInnerMultMatrix(this, op);
    
    /** Matrix-vector multiplication. */
    def *[T2<:Tensor1[I2]] (op : VectorOp[I2,T2]) = MatrixInnerMultVector(this, op);
    
    /** Matrix solve. */
    def \[J,T2<:Tensor2[I1,J]] (op : MatrixOp[I1,J,T2]) = MatrixSolveMatrix(this, op);
    
    /** Matrix-vector solve. */
    def \[T2<:Tensor1[I1]] (op : VectorOp[I1,T2]) = MatrixSolveVector(this, op);
  }
  
  /** An underlying tensor. */
  case class TensorIdentity[I,T<:Tensor[I]](tensor : T) extends TensorOp[I,T] {
    override def domain = tensor.domain;
    override def value = tensor;
    override val working = tensor.copy.asInstanceOf[T];
    override def create[J](d : Domain[J]) = tensor.create(d);
  }
  
  /** An operator between a tensor and scalar. */
  abstract class TensorScalarOp[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorOp[I,T] {
    override def domain = tensor.domain;
    override def create[J](d : Domain[J]) = tensor.create(d);
  }
  
  /** An operation applied to a tensors values only. */
  abstract class TensorFunctionOp[I,T<:Tensor[I]](tensor : TensorOp[I,T]) extends TensorOp[I,T] {
    /** Function to apply to elements to compute new value. */
    def function(x : Double) : Double;
    
    override def domain = tensor.domain;
    override def create[J](d : Domain[J]) = tensor.create(d);
    override lazy val value = {
      val rv = tensor.working;
      rv.default = function(rv.default);
      for (e <- rv.activeDomain) {
        rv(e) = function(rv(e));
      }
      rv;
    }
  }
  
  /** An operator between two tensors defined on the same domain. */
  abstract class TensorTensorOp[I,T1<:Tensor[I],T2<:Tensor[I]](tensorA : TensorOp[I,T1], tensorB : TensorOp[I,T2]) extends TensorOp[I,T1] {
    if (tensorA.domain != tensorB.domain) throw new Predef.IllegalArgumentException;
    override def domain = tensorA.domain;
    override def create[J](d : Domain[J]) = tensorA.create(d);
  }
  
  abstract class TensorTensorFunctionOp[I,T1<:Tensor[I],T2<:Tensor[I]](tensorA : TensorOp[I,T1], tensorB : TensorOp[I,T2]) extends TensorTensorOp(tensorA, tensorB) {
    /** Function of paired elements to compute new value. */
    def function(a : Double, b : Double) : Double;
    
    override lazy val value = {
      val rv = tensorA.working;
      val tb = tensorB.value;
      rv.default = function(rv.default, tb.default);
      for (e <- rv.activeDomain ++ tb.activeDomain) {
        rv(e) = function(rv(e),tb(e));
      }
      rv;
    }
  }
  
  case class TensorPlusScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override def  + (s : Double) = TensorPlusScalar(tensor, scalar + s);
    override def  - (s : Double) = TensorPlusScalar(tensor, scalar - s);
    override lazy val value = {
      val rv = tensor.working;
      rv += scalar;
      rv;
    }
  }
  
  case class TensorMultScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override def * (s : Double) = TensorMultScalar(tensor, scalar * s);
    override def / (s : Double) = TensorMultScalar(tensor, scalar / s);
    override lazy val value = {
      val rv = tensor.working;
      rv *= scalar;
      rv;
    }
  }
  
  case class TensorPowScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorScalarOp(tensor,scalar) {
    override lazy val value = {
      val rv = tensor.working;
      rv ^= scalar;
      rv;
    }
  }
  
  case class TensorLTScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x < scalar) 1.0 else 0.0;
  }
  
  case class TensorGTScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x > scalar) 1.0 else 0.0;
  }
  
  case class TensorLTEScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x <= scalar) 1.0 else 0.0;
  }
  
  case class TensorGTEScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x >= scalar) 1.0 else 0.0;
  }

  case class TensorEqScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x == scalar) 1.0 else 0.0;
  }
  
  case class TensorNeScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x != scalar) 1.0 else 0.0;
  }
  
  case class TensorOrScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x != 0.0 || scalar != 0) 1.0 else 0.0;
  }
  
  case class TensorAndScalar[I,T<:Tensor[I]](tensor : TensorOp[I,T], scalar : Double) extends TensorFunctionOp[I,T](tensor) {
    override def function(x : Double) = if (x != 0.0 && scalar != 0) 1.0 else 0.0;
  }
  
  case class TensorLTTensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a < b) 1.0 else 0.0;
  }
  
  case class TensorGTTensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a > b) 1.0 else 0.0;
  }
  
  case class TensorLTETensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a <= b) 1.0 else 0.0;
  }
  
  case class TensorGTETensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a >= b) 1.0 else 0.0;
  }
  
  case class TensorEqTensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a == b) 1.0 else 0.0;
  }
  
  case class TensorNeTensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a != b) 1.0 else 0.0;
  }
  
  case class TensorAndTensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a != 0 && b != 0) 1.0 else 0.0;
  }
  
  case class TensorOrTensor[I,T1<:Tensor[I],T2<:Tensor[I]](t1 : TensorOp[I,T1], t2 : TensorOp[I,T2]) extends TensorTensorFunctionOp(t1,t2) {
    override def function(a : Double, b : Double) = if (a != 0 || b != 0) 1.0 else 0.0;
  }
  
  case class TensorPlusTensor[I,T1<:Tensor[I],T2<:Tensor[I]](tensorA : TensorOp[I,T1], tensorB : TensorOp[I,T2]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :+= tensorB;
      rv;
    }
  }
  
  case class TensorMinusTensor[I,T1<:Tensor[I],T2<:Tensor[I]](tensorA : TensorOp[I,T1], tensorB : TensorOp[I,T2]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :-= tensorB;
      rv;
    }
  }
  
  case class TensorMultTensor[I,T1<:Tensor[I],T2<:Tensor[I]](tensorA : TensorOp[I,T1], tensorB : TensorOp[I,T2]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :*= tensorB;
      rv;
    }
  }
  
  case class TensorDivTensor[I,T1<:Tensor[I],T2<:Tensor[I]](tensorA : TensorOp[I,T1], tensorB : TensorOp[I,T2]) extends TensorTensorOp(tensorA,tensorB) {
    override lazy val value = {
      val rv = tensorA.working;
      rv :/= tensorB;
      rv;
    }
  }
  
  case class VectorIdentity[I,T<:Tensor1[I]](tensor1 : T) extends TensorIdentity[I,T](tensor1) with VectorOp[I,T] {
    override def domain1 = tensor1.domain;
    override def value1 = tensor1;
  }
  
  case class MatrixIdentity[I,J,T<:Tensor2[I,J]](tensor2 : T) extends TensorIdentity[(I,J),T](tensor2) with MatrixOp[I,J,T] {
    override def domain2 = tensor2.domain;
    override def value2 = tensor2;
  }
  
  case class VectorToRow[I,T<:Tensor1[I]](op : VectorOp[I,T]) extends RowVectorOp[I,T] {
    override def domain1 = op.domain1;
    override def value1 = op.value1;
    override def working = op.working;
    override def create[J](d : Domain[J]) = op.create(d);
  }
  
  case class VectorToCol[I,T<:Tensor1[I]](op : RowVectorOp[I,T]) extends VectorOp[I,T] {
    override def domain1 = op.domain1;
    override def value1 = op.value1;
    override def working = op.working;
    override def create[J](d : Domain[J]) = op.create(d);
  }
  
  case class MatrixTranspose[A,B,T<:Tensor2[A,B]](op : MatrixOp[A,B,T]) extends MatrixOp[B,A,Tensor2[B,A]] {
    override lazy val domain2 = op.domain2.transpose;
    override lazy val value2 = op.value2.transpose;
    override def create[J](d : Domain[J]) = op.create(d);
  }
  
  case class MatrixInnerMultMatrix[A1,INNER,B2,T1<:Tensor2[A1,INNER],T2<:Tensor2[INNER,B2]](t1 : MatrixOp[A1,INNER,T1], t2 : MatrixOp[INNER,B2,T2]) extends MatrixOp[A1,B2,Tensor2[A1,B2]] {
    if (t1.domain2._2 != t2.domain2._1) throw new Predef.IllegalArgumentException;
    override lazy val domain2 = Domain2(t1.domain2._1, t2.domain2._2);
    override lazy val value2 : Tensor2[A1,B2] = {
      val innerDomain = t1.domain2._2;
      val t1v = t1.value;
      val t2v = t2.value;
      val rv = create(domain2).asInstanceOf[Tensor2[A1,B2]];
      for (i <- domain2._1; j <- domain2._2) {
        rv(i,j) = t1v.getRow(i) dot t2v.getCol(j);
      }
      rv;
    }
    override def create[J](d : Domain[J]) = t1.create(d);
  }
 
  case class MatrixInnerMultVector[I,J,T1<:Tensor2[I,J],T2<:Tensor1[J]](matrix : MatrixOp[I,J,T1], vector : VectorOp[J,T2]) extends VectorOp[I,Tensor1[I]] {
    if (matrix.domain2._2 != vector.domain1) throw new DomainException;
    override def domain1 = matrix.domain2._1;
    override lazy val value1 : Tensor1[I] = {
      val mv = matrix.value;
      val vv = vector.value;
      val rv = create(domain1).asInstanceOf[Tensor1[I]];
      for (i <- domain1) {
        rv(i) = mv.getRow(i) dot vv;
      }
      rv;
    }
    override def create[J](d : Domain[J]) = vector.create(d);
  }
  
  case class VectorInnerMultMatrix[I,J,T1<:Tensor1[I],T2<:Tensor2[I,J]](vector : RowVectorOp[I,T1], matrix : MatrixOp[I,J,T2]) extends RowVectorOp[J,Tensor1[J]] {
    if (vector.domain1 != matrix.domain2._1) throw new DomainException;
    override def domain1 = matrix.domain2._2;
    override lazy val value1 : Tensor1[J] = {
      val vv = vector.value;
      val mv = matrix.value;
      val rv = vv.create(domain1).asInstanceOf[Tensor1[J]];
      for (j <- domain1) {
        rv(j) = vv dot mv.getCol(j);
      }
      rv;
    }
    override def create[J](d : Domain[J]) = vector.create(d);
  }

  case class VectorOuterMultVector[I,T1<:Tensor1[I],T2<:Tensor1[I]](v1 : VectorOp[I,T1], v2 : RowVectorOp[I,T2]) extends MatrixOp[I,I,Tensor2[I,I]] {
    if (v1.domain != v2.domain) throw new DomainException;
    override def domain2 = Domain2(v1.domain1,v1.domain1);
    override lazy val value2 : Tensor2[I,I] = {
      val v1v = v1.value;
      val v2v = v2.value;
      val rv = v1v.create(domain2).asInstanceOf[Tensor2[I,I]];
      for (i <- v1.domain; j <- v1.domain) {
        rv(i,j) = v1v(i) * v2v(j);
      }
      rv;
    }
    override def create[J](d : Domain[J]) = v1.create(d);
  }
  
  case class MatrixSolveMatrix[K,I,J,T1<:Tensor2[K,I],T2<:Tensor2[K,J]](m1 : MatrixOp[K,I,T1], m2 : MatrixOp[K,J,T2]) extends MatrixOp[I,J,Tensor2[I,J]] {
    if (m1.domain2._1 != m2.domain2._1) throw new DomainException;
    override def create[J](d : Domain[J]) = m1.create(d);
    override def domain2 = Domain2(m1.domain2._2, m2.domain2._2);
    override lazy val value2 : Tensor2[I,J] = {
      val X = m1.create(domain2);
      if (!X.isInstanceOf[scalala.tensor.MatrixMatrixSolver[_,_]]) {
        throw new UnsupportedOperationException("Type "+X.getClass+" does not support matrix solving");
      }
      X := this;
      X.asInstanceOf[Tensor2[I,J]];
    }
  }
  
  case class MatrixSolveVector[I,J,T1<:Tensor2[I,J],T2<:Tensor1[I]](m : MatrixOp[I,J,T1], v : VectorOp[I,T2]) extends VectorOp[J,Tensor1[J]] {
    if (m.domain2._1 != v.domain1) throw new DomainException;
    override def create[J](d : Domain[J]) = v.create(d);
    override def domain1 = m.domain2._2;
    override lazy val value1 : Tensor1[J] = {
      val X = v.create(domain1); // create a column matrix
      if (!X.isInstanceOf[scalala.tensor.MatrixVectorSolver[_]]) {
        throw new UnsupportedOperationException("Type "+X.getClass+" does not support matrix solving");
      }
      X := this;
      X.asInstanceOf[Tensor1[J]];
    } 
  }
