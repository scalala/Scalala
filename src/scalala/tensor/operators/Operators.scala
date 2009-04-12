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
import scalala.tensor.{Tensor, Tensor1, Tensor2, Vector, Matrix};

/**
 * Implicit conversions between TensorOp's and Tensors.
 * 
 * @author dramage
 */
trait OperatorImplicits {
  /** Implicit promotion to a ScalarOp for tensor operations. */
  implicit def iScalarOp[I](s : Double) = ScalarOp(s);
  implicit def iScalarOp[I](s : Float)  = ScalarOp(s);
  implicit def iScalarOp[I](s : Short)  = ScalarOp(s);
  implicit def iScalarOp[I](s : Int)    = ScalarOp(s);
  implicit def iScalarOp[I](s : Long)   = ScalarOp(s);
  
  /** Implicitly promotes a Tensor to a TensorIdentity operation. */
  implicit def iTensorOp[I](t : Tensor[I]) : TensorOp[I] =
    TensorIdentity(t);
  
  /** Implicitly promotes a Tensor1 to a VectorOp */
  implicit def iTensor1Op[I](t : Tensor1[I]) : VectorOp[I] =
    VectorIdentity(t);
  
  /** Implicitly promotes a Vector to a VectorOp */
  implicit def iVectorOp(t : Vector) : VectorOp[Int] =
    VectorIdentity(t);
  
  /** Implicitly promotes a Tensor2 to a MatrixOp */
  implicit def iTensor2Op[I,J](t : Tensor2[I,J]) : MatrixOp[I,J] =
    MatrixIdentity(t);
  
  /** Implicitly promotes a Matrix to a MatrixOp */
  implicit def iMatrixOp(t : Matrix) : MatrixOp[Int,Int] =
    MatrixIdentity(t);
  
  /** Implicitly promotes a TensorOp to its Tensor value. */
  implicit def iTensor[I](op : TensorOp[I]) : Tensor[I] = op.value;
}

object OperatorImplicits extends OperatorImplicits { }

/**
 * A promoted type for scalars that supports operations with tensors.
 * 
 * @author dramage
 */
case class ScalarOp(s : Double) {
  import OperatorImplicits._;
  import TensorImplicits._;
  
  def + [I] (t : TensorOp[I]) = TensorPlusScalar(t, s);
  def - [I] (t : TensorOp[I]) = TensorPlusScalar(TensorNegation(t), s);
  def * [I] (t : TensorOp[I]) = TensorMultScalar(t, s);
  def / [I] (t : TensorOp[I]) = ScalarDivTensor(s,t);
  def < [I] (t : TensorOp[I]) = TensorGTScalar(t,s);
  def > [I] (t : TensorOp[I]) = TensorLTScalar(t,s);
  def <=[I] (t : TensorOp[I]) = TensorGTEScalar(t,s);
  def >=[I] (t : TensorOp[I]) = TensorLTEScalar(t,s);
  def &&[I] (t : TensorOp[I]) = TensorAndScalar(t,s);
  def ||[I] (t : TensorOp[I]) = TensorOrScalar(t,s);
  
  def + [I] (t : Tensor[I]) = TensorPlusScalar(t, s);
  def - [I] (t : Tensor[I]) = TensorPlusScalar(TensorNegation(t), s);
  def * [I] (t : Tensor[I]) = TensorMultScalar(t, s);
  def / [I] (t : Tensor[I]) = ScalarDivTensor(s,t);
  def < [I] (t : Tensor[I]) = TensorGTScalar(t,s);
  def > [I] (t : Tensor[I]) = TensorLTScalar(t,s);
  def <=[I] (t : Tensor[I]) = TensorGTEScalar(t,s);
  def >=[I] (t : Tensor[I]) = TensorLTEScalar(t,s);
  def &&[I] (t : Tensor[I]) = TensorAndScalar(t,s);
  def ||[I] (t : Tensor[I]) = TensorOrScalar(t,s);
  
  def + [I] (t : Tensor1[I]) = TensorPlusScalar[I](t, s);
  def - [I] (t : Tensor1[I]) = TensorPlusScalar[I](-t, s);
  def * [I] (t : Tensor1[I]) = TensorMultScalar[I](t, s);
  def / [I] (t : Tensor1[I]) = ScalarDivTensor[I](s,t);
  def < [I] (t : Tensor1[I]) = TensorGTScalar[I](t,s);
  def > [I] (t : Tensor1[I]) = TensorLTScalar[I](t,s);
  def <=[I] (t : Tensor1[I]) = TensorGTEScalar[I](t,s);
  def >=[I] (t : Tensor1[I]) = TensorLTEScalar[I](t,s);
  def &&[I] (t : Tensor1[I]) = TensorAndScalar[I](t,s);
  def ||[I] (t : Tensor1[I]) = TensorOrScalar[I](t,s);
  
  def + (t : Vector) = TensorPlusScalar(t, s);
  def - (t : Vector) = TensorPlusScalar(-t, s);
  def * (t : Vector) = TensorMultScalar(t, s);
  def / (t : Vector) = ScalarDivTensor(s,t);
  def < (t : Vector) = TensorGTScalar(t,s);
  def > (t : Vector) = TensorLTScalar(t,s);
  def <=(t : Vector) = TensorGTEScalar(t,s);
  def >=(t : Vector) = TensorLTEScalar(t,s);
  def &&(t : Vector) = TensorAndScalar(t,s);
  def ||(t : Vector) = TensorOrScalar(t,s);
  
  def + [I,J] (t : Tensor2[I,J]) = TensorPlusScalar(t, s);
  def - [I,J] (t : Tensor2[I,J]) = TensorPlusScalar(-t, s);
  def * [I,J] (t : Tensor2[I,J]) = TensorMultScalar(t, s);
  def / [I,J] (t : Tensor2[I,J]) = ScalarDivTensor(s,t);
  def < [I,J] (t : Tensor2[I,J]) = TensorGTScalar(t,s);
  def > [I,J] (t : Tensor2[I,J]) = TensorLTScalar(t,s);
  def <=[I,J] (t : Tensor2[I,J]) = TensorGTEScalar(t,s);
  def >=[I,J] (t : Tensor2[I,J]) = TensorLTEScalar(t,s);
  def &&[I,J] (t : Tensor2[I,J]) = TensorAndScalar(t,s);
  def ||[I,J] (t : Tensor2[I,J]) = TensorOrScalar(t,s);
  
  def + (t : Matrix) = TensorPlusScalar(t, s);
  def - (t : Matrix) = TensorPlusScalar(-t, s);
  def * (t : Matrix) = TensorMultScalar(t, s);
  def / (t : Matrix) = ScalarDivTensor(s,t);
  def < (t : Matrix) = TensorGTScalar(t,s);
  def > (t : Matrix) = TensorLTScalar(t,s);
  def <=(t : Matrix) = TensorGTEScalar(t,s);
  def >=(t : Matrix) = TensorLTEScalar(t,s);
  def &&(t : Matrix) = TensorAndScalar(t,s);
  def ||(t : Matrix) = TensorOrScalar(t,s);
  
  def +  (t : Array[Double]) = TensorPlusScalar(t, s);
  def -  (t : Array[Double]) = TensorPlusScalar(TensorNegation(t), s);
  def *  (t : Array[Double]) = TensorMultScalar(t, s);
  def /  (t : Array[Double]) = ScalarDivTensor(s,t);
  def <  (t : Array[Double]) = TensorGTScalar(t,s);
  def >  (t : Array[Double]) = TensorLTScalar(t,s);
  def <= (t : Array[Double]) = TensorGTEScalar(t,s);
  def >= (t : Array[Double]) = TensorLTEScalar(t,s);
  def && (t : Array[Double]) = TensorAndScalar(t,s);
  def || (t : Array[Double]) = TensorOrScalar(t,s);
}

//
// Tensor operators
//

/**
 * Operations applicable to any tensor: updates by a scalar and
 * pairwise arithmetic.
 */
trait TensorOp[I] {
  /** The value type of this tensor. */
  type Value <: Tensor[I];
    
  /** Returns the domain of the output of this Tensor. */
  def domain : Domain[I];
    
  /**
   * Returns the value of this operator in a Vector that might
   * be visible outside of the evaluator, i.e. that its value
   * should not be mutated.
   */
  def value : Value;

  /**
   * Returns the value of this operator in a vector than can safely
   * be over-written.
   */
  def working : Value = value;
    
  /** Creates a new tensor for the requested domain type. */
  def create[J](d : Domain[J]) : Tensor[J];
    
  /** Unary minus returns a tensor negation. */
  def unary_- : TensorOp[I] = TensorNegation(this);
    
  // scalar operators
  def  + (s : Double) = TensorPlusScalar(this, s);
  def  - (s : Double) = TensorPlusScalar(this, -s);
  def  * (s : Double) : TensorOp[I] = TensorMultScalar(this, s);
  def  / (s : Double) : TensorOp[I] = TensorMultScalar(this, 1.0 / s);
  def :^ (s : Double) = TensorPowScalar(this, s);
  def  < (s : Double) = TensorLTScalar(this, s);
  def  > (s : Double) = TensorGTScalar(this, s);
  def <= (s : Double) = TensorLTEScalar(this, s);
  def >= (s : Double) = TensorGTEScalar(this, s);
  def && (s : Double) = TensorAndScalar(this, s);
  def || (s : Double) = TensorOrScalar(this, s);
    
  /** Colon prefix is required to avoid conflation with .equals */
  def :== (s : Double) = TensorEqScalar(this, s);
    
  /** Colon prefix is required to avoid conflation with .equals */
  def :!= (s : Double) = TensorNeScalar(this, s);
    
  // tensor operators
  def :+  (op : TensorOp[I]) = TensorPlusTensor(this, op);
  def :-  (op : TensorOp[I]) = TensorMinusTensor(this, op);
  def :*  (op : TensorOp[I]) = TensorMultTensor(this, op);
  def :/  (op : TensorOp[I]) = TensorDivTensor(this, op);
  def :<  (op : TensorOp[I]) = TensorLTTensor(this, op);
  def :>  (op : TensorOp[I]) = TensorGTTensor(this, op);
  def :<= (op : TensorOp[I]) = TensorLTETensor(this, op);
  def :>= (op : TensorOp[I]) = TensorGTETensor(this, op);
  def :== (op : TensorOp[I]) = TensorEqTensor(this, op);
  def :!= (op : TensorOp[I]) = TensorNeTensor(this, op);
  def :&& (op : TensorOp[I]) = TensorAndTensor(this, op);
  def :|| (op : TensorOp[I]) = TensorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  def + (op : TensorOp[I]) = this :+ op;
  /** Fixed alias for :- */
  def - (op : TensorOp[I]) = this :- op;
  /** Fixed alias for :< */
  def < (op : TensorOp[I]) = this :< op;
  /** Fixed alias for :> */
  def > (op : TensorOp[I]) = this :> op;
  /** Fixed alias for :<= */
  def <= (op : TensorOp[I]) = this :<= op;
  /** Fixed alias for :>= */
  def >= (op : TensorOp[I]) = this :>= op;
  /** Fixed alias for :&& */
  def && (op : TensorOp[I]) = this :&& op;
  /** Fixed alias for :|| */
  def || (op : TensorOp[I]) = this :|| op;
}
  
/** A reference to an underlying tensor. */
case class TensorIdentity[I](tensor : Tensor[I]) extends TensorOp[I] {
  type Value = tensor.type;
  override def domain = tensor.domain;
  override def value = tensor;
  override def working : Value = tensor.copy.asInstanceOf[Value];
  override def create[J](d : Domain[J]) = tensor.create(d);
}
  
/** A reference to an underlying TensorOp. */
abstract case class TensorReferenceOp[I](tensor : TensorOp[I]) extends TensorOp[I] {
  type Value = tensor.Value;
  override def domain = tensor.domain;
  override def create[J](d : Domain[J]) = tensor.create(d);
}
  
/** An operator between a tensor and scalar. */
abstract case class TensorScalarOp[I](override val tensor : TensorOp[I], scalar : Double) extends TensorReferenceOp[I](tensor) { }
  
/** An operation applied to a tensor's values. */
abstract case class TensorFunctionOp[I](override val tensor : TensorOp[I]) extends TensorReferenceOp[I](tensor) {
  /** Function to apply to elements to compute new value. */
  def function(x : Double) : Double;
    
  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = function(rv.default);
    rv(rv.activeDomain) = function _;
    rv.asInstanceOf[Value];
  }
}
  
/** An operator between two tensors defined on the same domain. */
abstract class TensorTensorOp[I](override val tensor : TensorOp[I], val tensorB : TensorOp[I]) extends TensorReferenceOp[I](tensor) {
  if (tensor.domain != tensorB.domain) throw new DomainException("Tensors have incompatible domain");
}
  
abstract class TensorTensorFunctionOp[I](tensorA : TensorOp[I], tensorB : TensorOp[I]) extends TensorTensorOp(tensorA, tensorB) {
  /** Function of paired elements to compute new value. */
  def function(a : Double, b : Double) : Double;
    
  override lazy val value : Value = {
    val rv = tensorA.working;
    val tb = tensorB.value;
    rv.default = function(rv.default, tb.default);
    for (e <- rv.activeDomain ++ tb.activeDomain) {
      rv(e) = function(rv(e),tb(e));
    }
    rv.asInstanceOf[Value];
  }
}
  
case class TensorNegation[I](override val tensor : TensorOp[I]) extends TensorReferenceOp[I](tensor) {
  override def value = negated;
    
  lazy val negated : Value = {
    tensor match {
      case TensorNegation(n) =>
        n.value.asInstanceOf[Value];
      case _ =>
        val rv = tensor.working;
        rv *= -1;
        rv.asInstanceOf[Value];
    }
  }
    
  override def unary_- = tensor;
}
  
case class TensorPlusScalar[I](override val tensor : TensorOp[I], override val scalar : Double) extends TensorScalarOp(tensor,scalar) {
  override def  + (s : Double) = TensorPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = TensorPlusScalar(tensor, scalar - s);
  lazy val _value : Value = {
    val rv = tensor.working;
    rv += scalar;
    rv.asInstanceOf[Value];
  }
  override def value = _value;
}
  
case class TensorMultScalar[I](override val tensor : TensorOp[I], override val scalar : Double) extends TensorScalarOp(tensor,scalar) {
  override def * (s : Double) = TensorMultScalar(tensor, scalar * s);
  override def / (s : Double) = TensorMultScalar(tensor, scalar / s);
  override lazy val value : Value = {
    val rv = tensor.working;
    rv *= scalar;
    rv.asInstanceOf[Value];
  }
}
  
case class ScalarDivTensor[I](override val scalar : Double, override val tensor : TensorOp[I]) extends TensorScalarOp(tensor, scalar) {
  override def * (s : Double) = ScalarDivTensor(scalar * s, tensor);
  override def / (s : Double) = ScalarDivTensor(scalar / s, tensor);
  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = scalar / rv.default;
    rv(rv.activeDomain) = ((x:Double) => scalar / x);
    rv.asInstanceOf[Value];
  }
}
  
case class TensorPowScalar[I](override val tensor : TensorOp[I], override val scalar : Double) extends TensorScalarOp(tensor,scalar) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv ^= scalar;
    rv.asInstanceOf[Value];
  }
}
  
case class TensorLTScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x < scalar) 1.0 else 0.0;
}
  
case class TensorGTScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x > scalar) 1.0 else 0.0;
}
  
case class TensorLTEScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x <= scalar) 1.0 else 0.0;
}
  
case class TensorGTEScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x >= scalar) 1.0 else 0.0;
}

case class TensorEqScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x == scalar) 1.0 else 0.0;
}
  
case class TensorNeScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x != scalar) 1.0 else 0.0;
}
  
case class TensorOrScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x != 0.0 || scalar != 0) 1.0 else 0.0;
}
  
case class TensorAndScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorFunctionOp[I](tensor) {
  override def function(x : Double) = if (x != 0.0 && scalar != 0) 1.0 else 0.0;
}

case class TensorPlusTensor[I](override val tensor : TensorOp[I], override val tensorB : TensorOp[I]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :+= tensorB;
    rv.asInstanceOf[Value];
  }
}
  
case class TensorMinusTensor[I](override val tensor : TensorOp[I], override val tensorB : TensorOp[I]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :-= tensorB;
    rv.asInstanceOf[Value];
  }
}
  
case class TensorMultTensor[I](override val tensor : TensorOp[I], override val tensorB : TensorOp[I]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :*= tensorB;
    rv.asInstanceOf[Value];
  }
}
  
case class TensorDivTensor[I](override val tensor : TensorOp[I], override val tensorB : TensorOp[I]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :/= tensorB;
    rv.asInstanceOf[Value];
  }
}
  
case class TensorLTTensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a < b) 1.0 else 0.0;
}
  
case class TensorGTTensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a > b) 1.0 else 0.0;
}
  
case class TensorLTETensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a <= b) 1.0 else 0.0;
}
  
case class TensorGTETensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a >= b) 1.0 else 0.0;
}
  
case class TensorEqTensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a == b) 1.0 else 0.0;
}
  
case class TensorNeTensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != b) 1.0 else 0.0;
}
  
case class TensorAndTensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 && b != 0) 1.0 else 0.0;
}
  
case class TensorOrTensor[I](t1 : TensorOp[I], t2 : TensorOp[I]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 || b != 0) 1.0 else 0.0;
}


//
// Vector operations
//

/** Extra operators for Tensor1 implementations.  A column. */
trait VectorOp[I] extends TensorOp[I] {
  type Value <: Tensor1[I];
  
  def domain : Domain1[I];
    
  /** Transposes this matrix. */
  def t = VectorToRow(this);
    
  override def unary_- : VectorOp[I] = this match {
    case VectorNegation(n) => n;
    case _ => VectorNegation(this);
  }
    
  /** Vector-vector multiplication. */
  def * (op : RowVectorOp[I]) = VectorOuterMultVector(this, op);
  
  //
  // type-narrowing
  //
  
  // typed scalar operators
  override def  + (s : Double) = VectorPlusScalar(this, s);
  override def  - (s : Double) = VectorPlusScalar(this, -s);
  override def  * (s : Double) : VectorOp[I] = VectorMultScalar(this, s);
  override def  / (s : Double) : VectorOp[I] = VectorMultScalar(this, 1.0 / s);
  override def :^ (s : Double) = VectorPowScalar(this, s);
  override def  < (s : Double) = VectorLTScalar(this, s);
  override def  > (s : Double) = VectorGTScalar(this, s);
  override def <= (s : Double) = VectorLTEScalar(this, s);
  override def >= (s : Double) = VectorGTEScalar(this, s);
  override def && (s : Double) = VectorAndScalar(this, s);
  override def || (s : Double) = VectorOrScalar(this, s);
  override def :== (s : Double) = VectorEqScalar(this, s);
  override def :!= (s : Double) = VectorNeScalar(this, s);
  
  // typed tensor operators
  override def :+  (op : TensorOp[I]) = VectorPlusTensor(this, op);
  override def :-  (op : TensorOp[I]) = VectorMinusTensor(this, op);
  override def :*  (op : TensorOp[I]) = VectorMultTensor(this, op);
  override def :/  (op : TensorOp[I]) = VectorDivTensor(this, op);
  override def :<  (op : TensorOp[I]) = VectorLTTensor(this, op);
  override def :>  (op : TensorOp[I]) = VectorGTTensor(this, op);
  override def :<= (op : TensorOp[I]) = VectorLTETensor(this, op);
  override def :>= (op : TensorOp[I]) = VectorGTETensor(this, op);
  override def :== (op : TensorOp[I]) = VectorEqTensor(this, op);
  override def :!= (op : TensorOp[I]) = VectorNeTensor(this, op);
  override def :&& (op : TensorOp[I]) = VectorAndTensor(this, op);
  override def :|| (op : TensorOp[I]) = VectorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[I]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[I]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[I]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[I]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[I]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[I]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[I]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[I]) = this :|| op;
}

case class VectorIdentity[I](override val tensor : Tensor1[I]) extends TensorIdentity[I](tensor) with VectorOp[I] {
  override def domain = tensor.domain;
  override def value : Value = tensor.asInstanceOf[Value];
}

case class VectorToCol[I](op : RowVectorOp[I]) extends VectorOp[I] {
  override type Value = op.Value;
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : Domain[J]) = op.create(d);
}
  
case class VectorNegation[I](override val tensor : VectorOp[I]) extends TensorNegation[I](tensor) with VectorOp[I] {
  override def domain = tensor.domain;
  override def value = negated.asInstanceOf[Value];
}

case class VectorPlusScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorMultScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
  override def * (s : Double) = VectorMultScalar(tensor, scalar * s);
  override def / (s : Double) = VectorMultScalar(tensor, scalar / s);
}

case class VectorPowScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorLTScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorGTScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorLTEScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorGTEScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorAndScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorOrScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorEqScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorNeScalar[I](override val tensor : VectorOp[I], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorPlusTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorPlusTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorMinusTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorMinusTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorMultTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorMultTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorDivTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorDivTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorLTTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorLTTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorGTTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorGTTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorLTETensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorLTETensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorGTETensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorGTETensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorEqTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorEqTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorNeTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorNeTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorAndTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorAndTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}

case class VectorOrTensor[I](override val tensor : VectorOp[I], override val tensorB : TensorOp[I]) extends TensorOrTensor(tensor, tensorB) with VectorOp[I] {
  override def domain = tensor.domain;
}



//
// Row Vector operations
//

/** Extra operators for Tensor1 implementations as a row. */
trait RowVectorOp[I] extends TensorOp[I] {
  type Value <: Tensor1[I];
  
  override def domain : Domain1[I];
    
  /** Transposes this matrix. */
  def t = VectorToCol(this);
    
  override def unary_- : RowVectorOp[I] = this match {
    case RowVectorNegation(n) => n;
    case _ => RowVectorNegation(this);
  }
    
  /** Vector-matrix inner multiplication. */
  def *[J] (op : MatrixOp[I,J]) = VectorInnerMultMatrix(this, op);
    
  /** Vector-vector inner multiplication. */
  def * (op : VectorOp[I]) = this.value dot op.value;
  
  //
  // type-narrowing
  //
    
  // typed scalar operators
  override def  + (s : Double) = RowVectorPlusScalar(this, s);
  override def  - (s : Double) = RowVectorPlusScalar(this, -s);
  override def  * (s : Double) : RowVectorOp[I] = RowVectorMultScalar(this, s);
  override def  / (s : Double) : RowVectorOp[I] = RowVectorMultScalar(this, 1.0 / s);
  override def :^ (s : Double) = RowVectorPowScalar(this, s);
  override def  < (s : Double) = RowVectorLTScalar(this, s);
  override def  > (s : Double) = RowVectorGTScalar(this, s);
  override def <= (s : Double) = RowVectorLTEScalar(this, s);
  override def >= (s : Double) = RowVectorGTEScalar(this, s);
  override def && (s : Double) = RowVectorAndScalar(this, s);
  override def || (s : Double) = RowVectorOrScalar(this, s);
  override def :== (s : Double) = RowVectorEqScalar(this, s);
  override def :!= (s : Double) = RowVectorNeScalar(this, s);
  
  // typed tensor operators
  override def :+  (op : TensorOp[I]) = RowVectorPlusTensor(this, op);
  override def :-  (op : TensorOp[I]) = RowVectorMinusTensor(this, op);
  override def :*  (op : TensorOp[I]) = RowVectorMultTensor(this, op);
  override def :/  (op : TensorOp[I]) = RowVectorDivTensor(this, op);
  override def :<  (op : TensorOp[I]) = RowVectorLTTensor(this, op);
  override def :>  (op : TensorOp[I]) = RowVectorGTTensor(this, op);
  override def :<= (op : TensorOp[I]) = RowVectorLTETensor(this, op);
  override def :>= (op : TensorOp[I]) = RowVectorGTETensor(this, op);
  override def :== (op : TensorOp[I]) = RowVectorEqTensor(this, op);
  override def :!= (op : TensorOp[I]) = RowVectorNeTensor(this, op);
  override def :&& (op : TensorOp[I]) = RowVectorAndTensor(this, op);
  override def :|| (op : TensorOp[I]) = RowVectorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[I]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[I]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[I]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[I]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[I]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[I]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[I]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[I]) = this :|| op;
}
 
case class VectorToRow[I](op : VectorOp[I]) extends RowVectorOp[I] {
  type Value = op.Value;
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : Domain[J]) = op.create(d);
}

case class RowVectorNegation[I](override val tensor : RowVectorOp[I]) extends TensorNegation[I](tensor) with RowVectorOp[I] {
  override def domain = tensor.domain;
  override def value = negated.asInstanceOf[Value];
}

case class RowVectorPlusScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
};

case class RowVectorMultScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
  override def * (s : Double) = RowVectorMultScalar(tensor, scalar * s);
  override def / (s : Double) = RowVectorMultScalar(tensor, scalar / s);
}

case class RowVectorPowScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorLTScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorGTScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorLTEScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorGTEScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorAndScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorOrScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorEqScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
};

case class RowVectorNeScalar[I](override val tensor : RowVectorOp[I], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorPlusTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorPlusTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorMinusTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorMinusTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorMultTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorMultTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorDivTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorDivTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorLTTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorLTTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorGTTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorGTTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorLTETensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorLTETensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorGTETensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorGTETensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorEqTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorEqTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorNeTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorNeTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorAndTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorAndTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}

case class RowVectorOrTensor[I](override val tensor : RowVectorOp[I], override val tensorB : TensorOp[I]) extends TensorOrTensor(tensor, tensorB) with RowVectorOp[I] {
  override def domain = tensor.domain;
}



//
// Matrix operations
//

/** Extra operators for Tensor2 implementations. */
trait MatrixOp[I1,I2] extends TensorOp[(I1,I2)] {
  type Value <: Tensor2[I1,I2];
  type Transpose <: Tensor2[I2,I1];
  
  override def domain : Domain2[I1,I2];
    
  /** Transposes this tensor. */
  def t = MatrixTranspose(this);
    
  override def unary_- : MatrixOp[I1,I2] = this match {
    case MatrixNegation(n) => n.asInstanceOf[MatrixOp[I1,I2]];
    case _ => MatrixNegation(this);
  }
    
  /** Matrix-matrix multiplication. */
  def *[J] (op : MatrixOp[I2,J]) = MatrixInnerMultMatrix(this, op);
    
  /** Matrix-vector multiplication. */
  def * (op : VectorOp[I2]) = MatrixInnerMultVector(this, op);
    
  /** Matrix solve. */
  def \[J] (op : MatrixOp[I1,J]) = MatrixSolveMatrix(this, op);
    
  /** Matrix-vector solve. */
  def \ (op : VectorOp[I1]) = MatrixSolveVector(this, op);
  
  //
  // type-narrowing
  //
  
  // typed scalar operators
  override def  + (s : Double) = MatrixPlusScalar(this, s);
  override def  - (s : Double) = MatrixPlusScalar(this, -s);
  override def  * (s : Double) : MatrixOp[I1,I2] = MatrixMultScalar(this, s);
  override def  / (s : Double) : MatrixOp[I1,I2] = MatrixMultScalar(this, 1.0 / s);
  override def :^ (s : Double) = MatrixPowScalar(this, s);
  override def  < (s : Double) = MatrixLTScalar(this, s);
  override def  > (s : Double) = MatrixGTScalar(this, s);
  override def <= (s : Double) = MatrixLTEScalar(this, s);
  override def >= (s : Double) = MatrixGTEScalar(this, s);
  override def && (s : Double) = MatrixAndScalar(this, s);
  override def || (s : Double) = MatrixOrScalar(this, s);
  override def :== (s : Double) = MatrixEqScalar(this, s);
  override def :!= (s : Double) = MatrixNeScalar(this, s);
  
  // typed tensor operators
  override def :+  (op : TensorOp[(I1,I2)]) = MatrixPlusTensor(this, op);
  override def :-  (op : TensorOp[(I1,I2)]) = MatrixMinusTensor(this, op);
  override def :*  (op : TensorOp[(I1,I2)]) = MatrixMultTensor(this, op);
  override def :/  (op : TensorOp[(I1,I2)]) = MatrixDivTensor(this, op);
  override def :<  (op : TensorOp[(I1,I2)]) = MatrixLTTensor(this, op);
  override def :>  (op : TensorOp[(I1,I2)]) = MatrixGTTensor(this, op);
  override def :<= (op : TensorOp[(I1,I2)]) = MatrixLTETensor(this, op);
  override def :>= (op : TensorOp[(I1,I2)]) = MatrixGTETensor(this, op);
  override def :== (op : TensorOp[(I1,I2)]) = MatrixEqTensor(this, op);
  override def :!= (op : TensorOp[(I1,I2)]) = MatrixNeTensor(this, op);
  override def :&& (op : TensorOp[(I1,I2)]) = MatrixAndTensor(this, op);
  override def :|| (op : TensorOp[(I1,I2)]) = MatrixOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[(I1,I2)]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[(I1,I2)]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[(I1,I2)]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[(I1,I2)]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[(I1,I2)]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[(I1,I2)]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[(I1,I2)]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[(I1,I2)]) = this :|| op;
}
  
case class MatrixIdentity[I,J](override val tensor : Tensor2[I,J]) extends TensorIdentity[(I,J)](tensor) with MatrixOp[I,J] {
  override def domain = tensor.domain;
  override def value : Value = tensor.asInstanceOf[Value];
}
  
case class MatrixNegation[I,J](override val tensor : MatrixOp[I,J]) extends TensorNegation[(I,J)](tensor) with MatrixOp[I,J] {
  override def domain = tensor.domain;
  override def value = negated.asInstanceOf[Value];
}
  
case class MatrixTranspose[A,B](op : MatrixOp[A,B]) extends MatrixOp[B,A] {
  override type Value = op.Transpose;
  override type Transpose = op.Value;
    
  override lazy val domain = op.domain.transpose;
  override lazy val value : Value = op.value.transpose.asInstanceOf[Value];
  override def create[J](d : Domain[J]) = op.create(d);
}
  
case class MatrixInnerMultMatrix[A1,INNER,B2](t1 : MatrixOp[A1,INNER], t2 : MatrixOp[INNER,B2]) extends MatrixOp[A1,B2] {
  if (t1.domain._2 != t2.domain._1) throw new Predef.IllegalArgumentException;
    
  override type Value = Tensor2[A1,B2];
  override lazy val domain = Domain2(t1.domain._1, t2.domain._2);
  override lazy val value : Value = {
    val innerDomain = t1.domain._2;
    val t1v = t1.value;
    val t2v = t2.value;
    val rv = create(domain).asInstanceOf[Tensor2[A1,B2]];
    for (i <- domain._1; j <- domain._2) {
      rv(i,j) = t1v.getRow(i) dot t2v.getCol(j);
    }
    rv;
  }
  override def create[J](d : Domain[J]) = t1.create(d);
}
 
case class MatrixInnerMultVector[I,J](matrix : MatrixOp[I,J], vector : VectorOp[J]) extends VectorOp[I] {
  if (matrix.domain._2 != vector.domain) throw new DomainException;
    
  override type Value = Tensor1[I];
  override def domain = matrix.domain._1;
  override lazy val value : Value = {
    val mv = matrix.value;
    val vv = vector.value;
    val rv = create(domain).asInstanceOf[Tensor1[I]];
    for (i <- domain) {
      rv(i) = mv.getRow(i) dot vv;
    }
    rv.asInstanceOf[Value];
  }
  override def create[J](d : Domain[J]) = vector.create(d);
}
  
case class VectorInnerMultMatrix[I,J](vector : RowVectorOp[I], matrix : MatrixOp[I,J]) extends RowVectorOp[J] {
  if (vector.domain != matrix.domain._1) throw new DomainException;
    
  override type Value = Tensor1[J];
  override def domain = matrix.domain._2;
  override lazy val value : Tensor1[J] = {
    val vv = vector.value;
    val mv = matrix.value;
    val rv = vv.create(domain).asInstanceOf[Value];
    for (j <- domain) {
      rv(j) = vv dot mv.getCol(j);
    }
    rv;
  }
  override def create[J](d : Domain[J]) = vector.create(d);
}

case class VectorOuterMultVector[I](v1 : VectorOp[I], v2 : RowVectorOp[I]) extends MatrixOp[I,I] {
  if (v1.domain != v2.domain) throw new DomainException;
    
  override type Value = Tensor2[I,I];
  override def domain = Domain2(v1.domain,v1.domain);
  override lazy val value : Value = {
    val v1v = v1.value;
    val v2v = v2.value;
    val rv = v1v.create(domain).asInstanceOf[Value];
    for (i <- v1.domain; j <- v1.domain) {
      rv(i,j) = v1v(i) * v2v(j);
    }
    rv;
  }
  override def create[J](d : Domain[J]) = v1.create(d);
}
  
case class MatrixSolveMatrix[K,I,J](m1 : MatrixOp[K,I], m2 : MatrixOp[K,J]) extends MatrixOp[I,J] {
  if (m1.domain._1 != m2.domain._1) throw new DomainException;
    
  override type Value = Tensor2[I,J];
  override def create[J](d : Domain[J]) = m1.create(d);
  override def domain = Domain2(m1.domain._2, m2.domain._2);
  override lazy val value : Value = {
    val X = m1.create(domain).asInstanceOf[Value];
    if (!X.isInstanceOf[scalala.tensor.MatrixMatrixSolver[_,_]]) {
      throw new UnsupportedOperationException("Type "+X.getClass+" does not support matrix solving");
    }
    X := this;
    X;
  }
}
  
case class MatrixSolveVector[I,J](m : MatrixOp[I,J], v : VectorOp[I]) extends VectorOp[J] {
  if (m.domain._1 != v.domain) throw new DomainException;
    
  override type Value = Tensor1[J];
  override def create[J](d : Domain[J]) = v.create(d);
  override def domain = m.domain._2;
  override lazy val value : Value = {
    val X = v.create(domain).asInstanceOf[Value];
    if (!X.isInstanceOf[scalala.tensor.MatrixVectorSolver[_]]) {
      throw new UnsupportedOperationException("Type "+X.getClass+" does not support matrix solving");
    }
    X := this;
    X;
  }
}

case class MatrixPlusScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
};

case class MatrixMultScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
  override def * (s : Double) = MatrixMultScalar(tensor, scalar * s);
  override def / (s : Double) = MatrixMultScalar(tensor, scalar / s);
}

case class MatrixPowScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixLTScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixGTScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixLTEScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixGTEScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixAndScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixOrScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixEqScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
};

case class MatrixNeScalar[I,J](override val tensor : MatrixOp[I,J], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixPlusTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorPlusTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixMinusTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorMinusTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixMultTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorMultTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixDivTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorDivTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixLTTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorLTTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixGTTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorGTTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixLTETensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorLTETensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixGTETensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorGTETensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixEqTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorEqTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixNeTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorNeTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixAndTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorAndTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}

case class MatrixOrTensor[I,J](override val tensor : MatrixOp[I,J], override val tensorB : TensorOp[(I,J)]) extends TensorOrTensor(tensor, tensorB) with MatrixOp[I,J] {
  override def domain = tensor.domain;
}


/*
package dense {
  import scalala.tensor.dense._;
  
  trait DenseMatrixOp extends MatrixOp[Int,Int] {
    type Value = DenseMatrix;
    
    def rows = domain._1.asInstanceOf[IntSpanDomain].end;
    def cols = domain._2.asInstanceOf[IntSpanDomain].end;

    // override def t = DenseMatrixTranspose(this);
    
    override def create[J](domain : Domain[J]) = 
      new DenseMatrix(new Array[Double](0),0,0).create(domain);
  }
  
  abstract case class DenseMatrixTranspose(op : DenseMatrixOp) extends DenseMatrixOp {
  }
  
  //c <- alpha * a * b + beta * c
  case class GEMM(alpha : Double, a : DenseMatrixOp, b : DenseMatrixOp, beta : Double, c : DenseMatrixOp) extends DenseMatrixOp {
    if ((a.rows != c.rows) ||
        (a.cols != b.rows) ||
        (b.cols != c.cols))
      throw new IllegalArgumentException("Wrong dimensions for matrix multiplication");
    
    override def domain = c.domain;
    override def value = {
      val (transA,_A) = a match {
        case DenseMatrixTranspose(aT) => (true, aT.value.asInstanceOf[DenseMatrix]);
        case _ =>                   (false, a.value.asInstanceOf[DenseMatrix]);
      }
        
      val (transB,_B) = b match {
        case DenseMatrixTranspose(bT) => (true, bT.value.asInstanceOf[DenseMatrix]);
        case _ =>                   (false, b.value.asInstanceOf[DenseMatrix]);
      }
      
      val _C = c.working;
      
      Numerics.blas.gemm(transA, transB, _C.rows, _C.cols, _A.cols,
                         alpha, _A.data, _A.rows, _B.data, _B.rows,
                         beta, _C.data, _C.rows);
      
      _C;
    }
  }
}
*/
