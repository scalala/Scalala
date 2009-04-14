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
  
  /** Implicitly promotes a Tensor1 to a Tensor1Op */
  implicit def iTensor1Op[I](t : Tensor1[I]) : Tensor1Op[I] =
    Tensor1Identity(t);
  
  /** Implicitly promotes a Vector to a VectorOp */
  implicit def iVectorOp(t : Vector) : VectorOp =
    VectorIdentity(t);
  
  implicit def iSparseBinaryVectorOp(t : scalala.tensor.sparse.SparseBinaryVector) =
    sparse.SparseBinaryVectorIdentity(t);
  
  /** Implicitly promotes a Tensor2 to a MatrixOp */
  implicit def iTensor2Op[I,J](t : Tensor2[I,J]) : MatrixOp[I,J] =
    MatrixIdentity(t);
  
  /** Implicitly promotes a Matrix to a MatrixOp */
  implicit def iMatrixOp(t : Matrix) : MatrixOp[Int,Int] =
    MatrixIdentity(t);
  
  /** Implicitly converts a TensorOp to its Tensor value. */
  implicit def iTensor[I](op : TensorOp[I]) : Tensor[I] = op.value;
  
  /** Implicitly converts a Tensor1Op to its Tensor1 value. */
  implicit def iTensor1[I](op : Tensor1Op[I]) : Tensor1[I] = op.value;
  
  /** Implicitly converts a RowTensor1Op to its Tensor1 value. */
  implicit def iTensor1[I](op : RowTensor1Op[I]) : Tensor1[I] = op.value;
  
  /** Implicitly converts a VectorOp to its Vector value. */
  implicit def iVector[I](op : VectorOp) : Vector = op.value;
  
  /** Implicitly converts a RowVectorOp to its Vector value. */
  implicit def iVector[I](op : RowVectorOp) : Vector = op.value;
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
  
case class TensorPlusScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorReferenceOp(tensor) {
  override def  + (s : Double) = TensorPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = TensorPlusScalar(tensor, scalar - s);
  lazy val _value : Value = {
    val rv = tensor.working;
    rv += scalar;
    rv.asInstanceOf[Value];
  }
  override def value = _value;
}
  
case class TensorMultScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorReferenceOp(tensor) {
  override def * (s : Double) = TensorMultScalar(tensor, scalar * s);
  override def / (s : Double) = TensorMultScalar(tensor, scalar / s);
  override lazy val value : Value = {
    val rv = tensor.working;
    rv *= scalar;
    rv.asInstanceOf[Value];
  }
}
  
case class ScalarDivTensor[I](scalar : Double, override val tensor : TensorOp[I]) extends TensorReferenceOp(tensor) {
  override def * (s : Double) = ScalarDivTensor(scalar * s, tensor);
  override def / (s : Double) = ScalarDivTensor(scalar / s, tensor);
  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = scalar / rv.default;
    rv(rv.activeDomain) = ((x:Double) => scalar / x);
    rv.asInstanceOf[Value];
  }
}
  
case class TensorPowScalar[I](override val tensor : TensorOp[I], scalar : Double) extends TensorReferenceOp(tensor) {
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
trait Tensor1Op[I] extends TensorOp[I] {
  type Value <: Tensor1[I];
  
  def domain : Domain1[I];
  
  /** Transposes this matrix. */
  def t : RowTensor1Op[I] = Tensor1ToRow(this);
    
  override def unary_- : Tensor1Op[I] = this match {
    case Tensor1Negation(n) => n;
    case _ => Tensor1Negation(this);
  }
    
  /** Vector-vector multiplication. */
  def * (op : RowTensor1Op[I]) = VectorOuterMultVector(this, op);
  
  //
  // type-narrowing
  //
  
  // typed scalar operators
  override def  + (s : Double) = Tensor1PlusScalar(this, s);
  override def  - (s : Double) = Tensor1PlusScalar(this, -s);
  override def  * (s : Double) : Tensor1Op[I] = Tensor1MultScalar(this, s);
  override def  / (s : Double) : Tensor1Op[I] = Tensor1MultScalar(this, 1.0 / s);
  override def :^ (s : Double) = Tensor1PowScalar(this, s);
  override def  < (s : Double) = Tensor1LTScalar(this, s);
  override def  > (s : Double) = Tensor1GTScalar(this, s);
  override def <= (s : Double) = Tensor1LTEScalar(this, s);
  override def >= (s : Double) = Tensor1GTEScalar(this, s);
  override def && (s : Double) = Tensor1AndScalar(this, s);
  override def || (s : Double) = Tensor1OrScalar(this, s);
  override def :== (s : Double) = Tensor1EqScalar(this, s);
  override def :!= (s : Double) = Tensor1NeScalar(this, s);
  
  // typed tensor operators
  override def :+  (op : TensorOp[I]) = Tensor1PlusTensor(this, op);
  override def :-  (op : TensorOp[I]) = Tensor1MinusTensor(this, op);
  override def :*  (op : TensorOp[I]) = Tensor1MultTensor(this, op);
  override def :/  (op : TensorOp[I]) = Tensor1DivTensor(this, op);
  override def :<  (op : TensorOp[I]) = Tensor1LTTensor(this, op);
  override def :>  (op : TensorOp[I]) = Tensor1GTTensor(this, op);
  override def :<= (op : TensorOp[I]) = Tensor1LTETensor(this, op);
  override def :>= (op : TensorOp[I]) = Tensor1GTETensor(this, op);
  override def :== (op : TensorOp[I]) = Tensor1EqTensor(this, op);
  override def :!= (op : TensorOp[I]) = Tensor1NeTensor(this, op);
  override def :&& (op : TensorOp[I]) = Tensor1AndTensor(this, op);
  override def :|| (op : TensorOp[I]) = Tensor1OrTensor(this, op);
  
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

case class Tensor1Identity[I](override val tensor : Tensor1[I]) extends TensorIdentity[I](tensor) with Tensor1Op[I] {
  override def domain = tensor.domain;
  override def value : Value = tensor.asInstanceOf[Value];
}

case class Tensor1ToCol[I](op : RowTensor1Op[I]) extends Tensor1Op[I] {
  override type Value = op.Value;
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : Domain[J]) = op.create(d);
}
  
case class Tensor1Negation[I](override val tensor : Tensor1Op[I]) extends TensorNegation[I](tensor) with Tensor1Op[I] {
  override def domain = tensor.domain;
  override def value = negated.asInstanceOf[Value];
}

case class Tensor1PlusScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
  override def  + (s : Double) = Tensor1PlusScalar(tensor, scalar + s);
  override def  - (s : Double) = Tensor1PlusScalar(tensor, scalar - s);
}

case class Tensor1MultScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
  override def * (s : Double) = Tensor1MultScalar(tensor, scalar * s);
  override def / (s : Double) = Tensor1MultScalar(tensor, scalar / s);
}

case class Tensor1PowScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1LTScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1GTScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1LTEScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1GTEScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1AndScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1OrScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1EqScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1NeScalar[I](override val tensor : Tensor1Op[I], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1PlusTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorPlusTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1MinusTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorMinusTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1MultTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorMultTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1DivTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorDivTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1LTTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorLTTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1GTTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorGTTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1LTETensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorLTETensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1GTETensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorGTETensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1EqTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorEqTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1NeTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorNeTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1AndTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorAndTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}

case class Tensor1OrTensor[I](override val tensor : Tensor1Op[I], override val tensorB : TensorOp[I]) extends TensorOrTensor(tensor, tensorB) with Tensor1Op[I] {
  override def domain = tensor.domain;
}



//
// Row Tensor1 operations
//

/** Extra operators for Tensor1 implementations as a row. */
trait RowTensor1Op[I] extends TensorOp[I] {
  type Value <: Tensor1[I];
  
  override def domain : Domain1[I];
    
  /** Transposes this matrix. */
  def t : Tensor1Op[I] = Tensor1ToCol(this);
    
  override def unary_- : RowTensor1Op[I] = this match {
    case RowTensor1Negation(n) => n;
    case _ => RowTensor1Negation(this);
  }
    
  /** Vector-matrix inner multiplication. */
  def *[J] (op : MatrixOp[I,J]) = VectorInnerMultMatrix(this, op);
    
  /** Vector-vector inner multiplication. */
  def * (op : Tensor1Op[I]) = this.value dot op.value;
  
  //
  // type-narrowing
  //
    
  // typed scalar operators
  override def  + (s : Double) = RowTensor1PlusScalar(this, s);
  override def  - (s : Double) = RowTensor1PlusScalar(this, -s);
  override def  * (s : Double) : RowTensor1Op[I] = RowTensor1MultScalar(this, s);
  override def  / (s : Double) : RowTensor1Op[I] = RowTensor1MultScalar(this, 1.0 / s);
  override def :^ (s : Double) = RowTensor1PowScalar(this, s);
  override def  < (s : Double) = RowTensor1LTScalar(this, s);
  override def  > (s : Double) = RowTensor1GTScalar(this, s);
  override def <= (s : Double) = RowTensor1LTEScalar(this, s);
  override def >= (s : Double) = RowTensor1GTEScalar(this, s);
  override def && (s : Double) = RowTensor1AndScalar(this, s);
  override def || (s : Double) = RowTensor1OrScalar(this, s);
  override def :== (s : Double) = RowTensor1EqScalar(this, s);
  override def :!= (s : Double) = RowTensor1NeScalar(this, s);
  
  // typed tensor operators
  override def :+  (op : TensorOp[I]) = RowTensor1PlusTensor(this, op);
  override def :-  (op : TensorOp[I]) = RowTensor1MinusTensor(this, op);
  override def :*  (op : TensorOp[I]) = RowTensor1MultTensor(this, op);
  override def :/  (op : TensorOp[I]) = RowTensor1DivTensor(this, op);
  override def :<  (op : TensorOp[I]) = RowTensor1LTTensor(this, op);
  override def :>  (op : TensorOp[I]) = RowTensor1GTTensor(this, op);
  override def :<= (op : TensorOp[I]) = RowTensor1LTETensor(this, op);
  override def :>= (op : TensorOp[I]) = RowTensor1GTETensor(this, op);
  override def :== (op : TensorOp[I]) = RowTensor1EqTensor(this, op);
  override def :!= (op : TensorOp[I]) = RowTensor1NeTensor(this, op);
  override def :&& (op : TensorOp[I]) = RowTensor1AndTensor(this, op);
  override def :|| (op : TensorOp[I]) = RowTensor1OrTensor(this, op);
  
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
 
case class Tensor1ToRow[I](op : Tensor1Op[I]) extends RowTensor1Op[I] {
  type Value = op.Value;
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : Domain[J]) = op.create(d);
}

case class RowTensor1Negation[I](override val tensor : RowTensor1Op[I]) extends TensorNegation[I](tensor) with RowTensor1Op[I] {
  override def domain = tensor.domain;
  override def value = negated.asInstanceOf[Value];
}

case class RowTensor1PlusScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
  override def  + (s : Double) = RowTensor1PlusScalar(tensor, scalar + s);
  override def  - (s : Double) = RowTensor1PlusScalar(tensor, scalar - s);
};

case class RowTensor1MultScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
  override def * (s : Double) = RowTensor1MultScalar(tensor, scalar * s);
  override def / (s : Double) = RowTensor1MultScalar(tensor, scalar / s);
}

case class RowTensor1PowScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1LTScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1GTScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1LTEScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1GTEScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1AndScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1OrScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1EqScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
};

case class RowTensor1NeScalar[I](override val tensor : RowTensor1Op[I], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1PlusTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorPlusTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1MinusTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorMinusTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1MultTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorMultTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1DivTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorDivTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1LTTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorLTTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1GTTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorGTTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1LTETensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorLTETensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1GTETensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorGTETensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1EqTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorEqTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1NeTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorNeTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1AndTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorAndTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}

case class RowTensor1OrTensor[I](override val tensor : RowTensor1Op[I], override val tensorB : TensorOp[I]) extends TensorOrTensor(tensor, tensorB) with RowTensor1Op[I] {
  override def domain = tensor.domain;
}


//
// Vector operations
//

trait VectorOp extends Tensor1Op[Int] {
  type Value <: Vector;
  
  /** Vector-vector multiplication. */
  def * (op : RowVectorOp) = VectorOuterMultVector(this, op);
  
  //
  // type-narrowing
  //
  
  // typed scalar operators
  override def  + (s : Double) = VectorPlusScalar(this, s);
  override def  - (s : Double) = VectorPlusScalar(this, -s);
  override def  * (s : Double) : VectorOp = VectorMultScalar(this, s);
  override def  / (s : Double) : VectorOp = VectorMultScalar(this, 1.0 / s);
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
  override def :+  (op : TensorOp[Int]) = VectorPlusTensor(this, op);
  override def :-  (op : TensorOp[Int]) = VectorMinusTensor(this, op);
  override def :*  (op : TensorOp[Int]) = VectorMultTensor(this, op);
  override def :/  (op : TensorOp[Int]) = VectorDivTensor(this, op);
  override def :<  (op : TensorOp[Int]) = VectorLTTensor(this, op);
  override def :>  (op : TensorOp[Int]) = VectorGTTensor(this, op);
  override def :<= (op : TensorOp[Int]) = VectorLTETensor(this, op);
  override def :>= (op : TensorOp[Int]) = VectorGTETensor(this, op);
  override def :== (op : TensorOp[Int]) = VectorEqTensor(this, op);
  override def :!= (op : TensorOp[Int]) = VectorNeTensor(this, op);
  override def :&& (op : TensorOp[Int]) = VectorAndTensor(this, op);
  override def :|| (op : TensorOp[Int]) = VectorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[Int]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[Int]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[Int]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[Int]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[Int]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[Int]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[Int]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[Int]) = this :|| op;
  
}

case class VectorIdentity(override val tensor : Vector) extends Tensor1Identity[Int](tensor) with VectorOp {
  override def value : Value = tensor.asInstanceOf[Value];
}

case class VectorToCol(op : RowVectorOp) extends VectorOp {
  override type Value = op.Value;
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : Domain[J]) = op.create(d);
}
  
case class VectorNegation(override val tensor : VectorOp) extends Tensor1Negation[Int](tensor) with VectorOp {
  override def value = negated.asInstanceOf[Value];
}

case class VectorPlusScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1PlusScalar(tensor, scalar) with VectorOp {
  override def  + (s : Double) = VectorPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = VectorPlusScalar(tensor, scalar - s);
}

case class VectorMultScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1MultScalar(tensor, scalar) with VectorOp {
  override def * (s : Double) = VectorMultScalar(tensor, scalar * s);
  override def / (s : Double) = VectorMultScalar(tensor, scalar / s);
}

case class VectorPowScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1PowScalar(tensor, scalar) with VectorOp {}

case class VectorLTScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1LTScalar(tensor, scalar) with VectorOp {}

case class VectorGTScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1GTScalar(tensor, scalar) with VectorOp {}

case class VectorLTEScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1LTEScalar(tensor, scalar) with VectorOp {}

case class VectorGTEScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1GTEScalar(tensor, scalar) with VectorOp {}

case class VectorAndScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1AndScalar(tensor, scalar) with VectorOp {}

case class VectorOrScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1OrScalar(tensor, scalar) with VectorOp {}

case class VectorEqScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1EqScalar(tensor, scalar) with VectorOp {}

case class VectorNeScalar(override val tensor : VectorOp, override val scalar : Double) extends Tensor1NeScalar(tensor, scalar) with VectorOp {}

case class VectorPlusTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1PlusTensor(tensor, tensorB) with VectorOp {}

case class VectorMinusTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1MinusTensor(tensor, tensorB) with VectorOp {}

case class VectorMultTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1MultTensor(tensor, tensorB) with VectorOp {}

case class VectorDivTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1DivTensor(tensor, tensorB) with VectorOp {}

case class VectorLTTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1LTTensor(tensor, tensorB) with VectorOp {}

case class VectorGTTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1GTTensor(tensor, tensorB) with VectorOp {}

case class VectorLTETensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1LTETensor(tensor, tensorB) with VectorOp {}

case class VectorGTETensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1GTETensor(tensor, tensorB) with VectorOp {}

case class VectorEqTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1EqTensor(tensor, tensorB) with VectorOp {}

case class VectorNeTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1NeTensor(tensor, tensorB) with VectorOp {}

case class VectorAndTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1AndTensor(tensor, tensorB) with VectorOp {}

case class VectorOrTensor(override val tensor : VectorOp, override val tensorB : TensorOp[Int]) extends Tensor1OrTensor(tensor, tensorB) with VectorOp {}



//
// Row Vector operations
//

/** Extra operators for Tensor1 implementations as a row. */
trait RowVectorOp extends RowTensor1Op[Int] {
  type Value <: Vector;
    
  /** Transposes this matrix. */
  override def t : VectorOp = VectorToCol(this);
    
  override def unary_- : RowVectorOp = this match {
    case RowVectorNegation(n) => n;
    case _ => RowVectorNegation(this);
  }
    
  /** Vector-matrix inner multiplication. */
  override def *[J] (op : MatrixOp[Int,J]) = VectorInnerMultMatrix(this, op);
  
  /** Vector-vector inner multiplication. */
  def * (op : VectorOp) = this.value dot op.value;
  
  //
  // type-narrowing
  //
    
  // typed scalar operators
  override def  + (s : Double) = RowVectorPlusScalar(this, s);
  override def  - (s : Double) = RowVectorPlusScalar(this, -s);
  override def  * (s : Double) : RowVectorOp = RowVectorMultScalar(this, s);
  override def  / (s : Double) : RowVectorOp = RowVectorMultScalar(this, 1.0 / s);
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
  override def :+  (op : TensorOp[Int]) = RowVectorPlusTensor(this, op);
  override def :-  (op : TensorOp[Int]) = RowVectorMinusTensor(this, op);
  override def :*  (op : TensorOp[Int]) = RowVectorMultTensor(this, op);
  override def :/  (op : TensorOp[Int]) = RowVectorDivTensor(this, op);
  override def :<  (op : TensorOp[Int]) = RowVectorLTTensor(this, op);
  override def :>  (op : TensorOp[Int]) = RowVectorGTTensor(this, op);
  override def :<= (op : TensorOp[Int]) = RowVectorLTETensor(this, op);
  override def :>= (op : TensorOp[Int]) = RowVectorGTETensor(this, op);
  override def :== (op : TensorOp[Int]) = RowVectorEqTensor(this, op);
  override def :!= (op : TensorOp[Int]) = RowVectorNeTensor(this, op);
  override def :&& (op : TensorOp[Int]) = RowVectorAndTensor(this, op);
  override def :|| (op : TensorOp[Int]) = RowVectorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[Int]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[Int]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[Int]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[Int]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[Int]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[Int]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[Int]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[Int]) = this :|| op;
}
 
case class VectorToRow(op : VectorOp) extends RowVectorOp {
  type Value = op.Value;
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : Domain[J]) = op.create(d);
}

case class RowVectorNegation(override val tensor : RowVectorOp) extends RowTensor1Negation[Int](tensor) with RowVectorOp {}

case class RowVectorPlusScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1PlusScalar(tensor, scalar) with RowVectorOp {
  override def  + (s : Double) = RowVectorPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = RowVectorPlusScalar(tensor, scalar - s);
}

case class RowVectorMultScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1MultScalar(tensor, scalar) with RowVectorOp {
  override def * (s : Double) = RowVectorMultScalar(tensor, scalar * s);
  override def / (s : Double) = RowVectorMultScalar(tensor, scalar / s);
}

case class RowVectorPowScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1PowScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorLTScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1LTScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorGTScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1GTScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorLTEScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1LTEScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorGTEScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1GTEScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorAndScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1AndScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorOrScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1OrScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorEqScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1EqScalar(tensor, scalar) with RowVectorOp {};

case class RowVectorNeScalar(override val tensor : RowVectorOp, override val scalar : Double) extends RowTensor1NeScalar(tensor, scalar) with RowVectorOp {}

case class RowVectorPlusTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1PlusTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorMinusTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1MinusTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorMultTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1MultTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorDivTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1DivTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorLTTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1LTTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorGTTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1GTTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorLTETensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1LTETensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorGTETensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1GTETensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorEqTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1EqTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorNeTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1NeTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorAndTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1AndTensor(tensor, tensorB) with RowVectorOp {}

case class RowVectorOrTensor(override val tensor : RowVectorOp, override val tensorB : TensorOp[Int]) extends RowTensor1OrTensor(tensor, tensorB) with RowVectorOp {}



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
  def * (op : Tensor1Op[I2]) = MatrixInnerMultVector(this, op);
  
  /** Matrix solve. */
  def \[J] (op : MatrixOp[I1,J]) = MatrixSolveMatrix(this, op);
  
  /** Matrix-vector solve. */
  def \ (op : Tensor1Op[I1]) = MatrixSolveVector(this, op);
  
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
 
case class MatrixInnerMultVector[I,J](matrix : MatrixOp[I,J], vector : Tensor1Op[J]) extends Tensor1Op[I] {
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
  
case class VectorInnerMultMatrix[I,J](vector : RowTensor1Op[I], matrix : MatrixOp[I,J]) extends RowTensor1Op[J] {
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

case class VectorOuterMultVector[I](v1 : Tensor1Op[I], v2 : RowTensor1Op[I]) extends MatrixOp[I,I] {
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
  
case class MatrixSolveVector[I,J](m : MatrixOp[I,J], v : Tensor1Op[I]) extends Tensor1Op[J] {
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
  override def  + (s : Double) = MatrixPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = MatrixPlusScalar(tensor, scalar - s);
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


package sparse {
  import scalala.tensor.sparse._;
  
  case class SparseBinaryVectorIdentity(vector : SparseBinaryVector) extends VectorOp {
    type Value = SparseVector;
    override def domain = vector.domain;
    lazy val _value = vector.toSparseVector
    override def value : Value = _value;
    override def working : Value = vector.toSparseVector;
    override def create[J](d : Domain[J]) = new SparseVector(0).create(d);
    
    override def t = SparseBinaryRowVectorIdentity(vector);
  }
  
  case class SparseBinaryRowVectorIdentity(vector : SparseBinaryVector) extends RowVectorOp {
    type Value = SparseVector;
    override def domain = vector.domain;
    lazy val _value = vector.toSparseVector;
    override def value : Value = _value;
    override def working : Value = vector.toSparseVector;
    override def create[J](d : Domain[J]) = new SparseVector(0).create(d);
    
    override def t = SparseBinaryVectorIdentity(vector);
    
    /** Vector-vector inner multiplication. */
    def * (op : SparseBinaryVectorIdentity) = this.vector dot op.vector;
  }
}

package dense {
  import scalala.tensor.dense._;
  
  trait DenseMatrixOp extends MatrixOp[Int,Int] {
    override type Value = DenseMatrix;
    override type Transpose = DenseMatrix;
    
    def rows = domain._1.asInstanceOf[IntSpanDomain].end;
    def cols = domain._2.asInstanceOf[IntSpanDomain].end;

    override def t = DenseMatrixTranspose(this);
    
    override def create[J](domain : Domain[J]) = 
      new DenseMatrix(0,0).create(domain);
  }
  
  case class DenseMatrixNegation(override val tensor : DenseMatrixOp) extends MatrixNegation[Int,Int](tensor) with DenseMatrixOp {}
  case class DenseMatrixTranspose(override val op : DenseMatrixOp) extends MatrixTranspose[Int,Int](op) with DenseMatrixOp {}
  
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
