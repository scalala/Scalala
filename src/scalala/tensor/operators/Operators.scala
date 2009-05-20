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

import scalala.collection.{MergeableSet, IntSpanSet, ProductSet, DomainException};
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
  implicit def iTensorOp[I](t : Tensor[I]) : TensorOp[I,Tensor[I]] =
    TensorIdentity(t);
  
  /** Implicitly promotes a Tensor1 to a Tensor1Op */
  implicit def iTensor1Op[I](t : Tensor1[I]) : Tensor1Op[I,Tensor1[I]] =
    Tensor1Identity(t);
  
  /** Implicitly promotes a Vector to a VectorOp */
  implicit def iVectorOp(t : Vector) : VectorOp[Vector] =
    VectorIdentity(t);
  
  implicit def iSparseBinaryVectorOp(t : scalala.tensor.sparse.SparseBinaryVector) =
    sparse.SparseBinaryVectorIdentity(t);
  
  /** Implicitly promotes a Tensor2 to a MatrixOp */
  implicit def iTensor2Op[I,J](t : Tensor2[I,J]) : MatrixOp[I,J,Tensor2[I,J]] =
    MatrixIdentity(t);
  
  /** Implicitly promotes a Matrix to a MatrixOp */
  implicit def iMatrixOp(t : Matrix) : MatrixOp[Int,Int,Matrix] =
    MatrixIdentity(t);
  
  /** Implicitly converts a TensorOp to its Tensor value. */
  implicit def iTensor[I,T<:Tensor[I]](op : TensorOp[I,T]) : T = op.value;
  
  /** Implicitly converts a Tensor1Op to its Tensor1 value. */
  implicit def iTensor1[I,T<:Tensor1[I]](op : Tensor1Op[I,T]) : T = op.value;
  
  /** Implicitly converts a RowTensor1Op to its Tensor1 value. */
  implicit def iTensor1[I,T<:Tensor1[I]](op : RowTensor1Op[I,T]) : T = op.value;
  
  /** Implicitly converts a VectorOp to its Vector value. */
  implicit def iVector[I,V<:Vector](op : VectorOp[V]) : V = op.value;
  
  /** Implicitly converts a RowVectorOp to its Vector value. */
  implicit def iVector[I,V<:Vector](op : RowVectorOp[V]) : V = op.value;
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
  
  def + [I] (t : TensorOp[I,Tensor[I]]) = TensorPlusScalar(t, s);
  def - [I] (t : TensorOp[I,Tensor[I]]) = TensorPlusScalar(TensorNegation(t), s);
  def * [I] (t : TensorOp[I,Tensor[I]]) = TensorMultScalar(t, s);
  def / [I] (t : TensorOp[I,Tensor[I]]) = ScalarDivTensor(s,t);
  def < [I] (t : TensorOp[I,Tensor[I]]) = TensorGTScalar(t,s);
  def > [I] (t : TensorOp[I,Tensor[I]]) = TensorLTScalar(t,s);
  def <=[I] (t : TensorOp[I,Tensor[I]]) = TensorGTEScalar(t,s);
  def >=[I] (t : TensorOp[I,Tensor[I]]) = TensorLTEScalar(t,s);
  def &&[I] (t : TensorOp[I,Tensor[I]]) = TensorAndScalar(t,s);
  def ||[I] (t : TensorOp[I,Tensor[I]]) = TensorOrScalar(t,s);
  
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
  
  def + [I] (t : Tensor1[I]) = TensorPlusScalar[I,Tensor1[I]](t, s);
  def - [I] (t : Tensor1[I]) = TensorPlusScalar[I,Tensor1[I]](-t, s);
  def * [I] (t : Tensor1[I]) = TensorMultScalar[I,Tensor1[I]](t, s);
  def / [I] (t : Tensor1[I]) = ScalarDivTensor[I,Tensor1[I]](s,t);
  def < [I] (t : Tensor1[I]) = TensorGTScalar[I,Tensor1[I]](t,s);
  def > [I] (t : Tensor1[I]) = TensorLTScalar[I,Tensor1[I]](t,s);
  def <=[I] (t : Tensor1[I]) = TensorGTEScalar[I,Tensor1[I]](t,s);
  def >=[I] (t : Tensor1[I]) = TensorLTEScalar[I,Tensor1[I]](t,s);
  def &&[I] (t : Tensor1[I]) = TensorAndScalar[I,Tensor1[I]](t,s);
  def ||[I] (t : Tensor1[I]) = TensorOrScalar[I,Tensor1[I]](t,s);
  
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
trait TensorOp[I,+Value<:Tensor[I]] {
  /** Returns the domain of the output of this Tensor. */
  def domain : MergeableSet[I];
    
  /**
   * Returns the value of this operator in a Vector that might
   * be visible outside of the evaluator, i.e. that its value
   * should not be mutated.
   */
  def value : Value;

  /**
   * Returns the value of this operator in a vector than can safely
   * be over-written.  Defaults to this.value but is override in
   * TensorIdentity and related operators.
   */
  def working : Value = value;
    
  /** Creates a new tensor for the requested domain type. */
  def create[J](d : MergeableSet[J]) : Tensor[J];
  
  /** Unary minus returns a tensor negation. */
  def unary_- : TensorOp[I,Value] = TensorNegation(this);
    
  // scalar operators
  def  + (s : Double) = TensorPlusScalar(this, s);
  def  - (s : Double) = TensorPlusScalar(this, -s);
  def  * (s : Double) : TensorOp[I,Value] = TensorMultScalar(this, s);
  def  / (s : Double) : TensorOp[I,Value] = TensorMultScalar(this, 1.0 / s);
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
  def :+  (op : TensorOp[I,Tensor[I]]) = TensorPlusTensor(this, op);
  def :-  (op : TensorOp[I,Tensor[I]]) = TensorMinusTensor(this, op);
  def :*  (op : TensorOp[I,Tensor[I]]) = TensorMultTensor(this, op);
  def :/  (op : TensorOp[I,Tensor[I]]) = TensorDivTensor(this, op);
  def :^  (op : TensorOp[I,Tensor[I]]) = TensorPowTensor(this, op);
  def :<  (op : TensorOp[I,Tensor[I]]) = TensorLTTensor(this, op);
  def :>  (op : TensorOp[I,Tensor[I]]) = TensorGTTensor(this, op);
  def :<= (op : TensorOp[I,Tensor[I]]) = TensorLTETensor(this, op);
  def :>= (op : TensorOp[I,Tensor[I]]) = TensorGTETensor(this, op);
  def :== (op : TensorOp[I,Tensor[I]]) = TensorEqTensor(this, op);
  def :!= (op : TensorOp[I,Tensor[I]]) = TensorNeTensor(this, op);
  def :&& (op : TensorOp[I,Tensor[I]]) = TensorAndTensor(this, op);
  def :|| (op : TensorOp[I,Tensor[I]]) = TensorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  def + (op : TensorOp[I,Tensor[I]]) = this :+ op;
  /** Fixed alias for :- */
  def - (op : TensorOp[I,Tensor[I]]) = this :- op;
  /** Fixed alias for :< */
  def < (op : TensorOp[I,Tensor[I]]) = this :< op;
  /** Fixed alias for :> */
  def > (op : TensorOp[I,Tensor[I]]) = this :> op;
  /** Fixed alias for :<= */
  def <= (op : TensorOp[I,Tensor[I]]) = this :<= op;
  /** Fixed alias for :>= */
  def >= (op : TensorOp[I,Tensor[I]]) = this :>= op;
  /** Fixed alias for :&& */
  def && (op : TensorOp[I,Tensor[I]]) = this :&& op;
  /** Fixed alias for :|| */
  def || (op : TensorOp[I,Tensor[I]]) = this :|| op;
  
  /**
   * TensorOp equality automatically compares this.value with other.value
   * if the other is an op.  This breaks hashCode semantics for different
   * expressions that evaluate to the same tensor (i.e. they may have
   * different hashCodes but will be equal).  However, these semantics
   * get rid of unreasonable expressions like:
   * 
   *   Vector(1,2,3)+1 != Vector(3,4,5)-1
   */
  //override def equals(other : Any) = other match {
  //  case tensor : Tensor[i] => tensor == this.value;
  //  case op : TensorOp[i] => 
  //   this.domain == op.domain && (
  //     super.equals(other) || (this.value == op.value));
  // case _ => super.equals(other);
  //}
}
  
/** A reference to an underlying tensor. */
case class TensorIdentity[I,+Value<:Tensor[I]](tensor : Value) extends TensorOp[I,Value] {
  override def domain = tensor.domain;
  override def value = tensor;
  override def working : Value = tensor.copy.asInstanceOf[Value];
  override def create[J](d : MergeableSet[J]) = tensor.create(d);
}
  
/** A reference to an underlying TensorOp. */
abstract case class TensorReferenceOp[I,+Value<:Tensor[I]](tensor : TensorOp[I,Value]) extends TensorOp[I,Value] {
  override def domain = tensor.domain;
  override def create[J](d : MergeableSet[J]) = tensor.create(d);
}
  
/** An operation applied to a tensor's values. */
abstract case class TensorFunctionOp[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value]) extends TensorReferenceOp[I,Value](tensor) {
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
abstract class TensorTensorOp[I,+V1<:Tensor[I],+V2<:Tensor[I]](override val tensor : TensorOp[I,V1], val tensorB : TensorOp[I,V2]) extends TensorReferenceOp[I,V1](tensor) {
  if (tensor.domain != tensorB.domain) throw new DomainException("Tensors have incompatible domain");
}
  
abstract class TensorTensorFunctionOp[I,+V1<:Tensor[I],+V2<:Tensor[I]](tensorA : TensorOp[I,V1], tensorB : TensorOp[I,V2]) extends TensorTensorOp(tensorA, tensorB) {
  /** Function of paired elements to compute new value. */
  def function(a : Double, b : Double) : Double;
    
  override lazy val value : V1 = {
    val rv = tensorA.working;
    val tb = tensorB.value;
    rv.default = function(rv.default, tb.default);
    for (e <- rv.activeDomain ++ tb.activeDomain) {
      rv(e) = function(rv(e),tb(e));
    }
    rv.asInstanceOf[V1];
  }
}
  
case class TensorNegation[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value]) extends TensorReferenceOp[I,Value](tensor) {
  override def value = negated;
    
  lazy val negated : Value = {
    tensor match {
      case TensorNegation(n) =>
        n.value.asInstanceOf[Value];
      case _ =>
        val rv = tensor.working;
        rv *= -1;
        rv;
    }
  }
    
  override def unary_- = tensor;
}
  
case class TensorPlusScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorReferenceOp(tensor) {
  override def  + (s : Double) = (tensor + (scalar + s)); // TensorPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = (tensor + (scalar - s)); // TensorPlusScalar(tensor, scalar - s);
  lazy val _value : Value = {
    val rv = tensor.working;
    rv += scalar;
    rv;
  }
  override def value = _value;
}
  
case class TensorMultScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorReferenceOp(tensor) {
  override def * (s : Double) = TensorMultScalar(tensor, scalar * s);
  override def / (s : Double) = TensorMultScalar(tensor, scalar / s);
  override lazy val value : Value = {
    val rv = tensor.working;
    rv *= scalar;
    rv;
  }
}
  
case class ScalarDivTensor[I,+Value<:Tensor[I]](scalar : Double, override val tensor : TensorOp[I,Value]) extends TensorReferenceOp(tensor) {
  override def * (s : Double) = ScalarDivTensor(scalar * s, tensor);
  override def / (s : Double) = ScalarDivTensor(scalar / s, tensor);
  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = scalar / rv.default;
    rv(rv.activeDomain) = ((x:Double) => scalar / x);
    rv;
  }
}
  
case class TensorPowScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorReferenceOp(tensor) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :^= scalar;
    rv;
  }
}
  
case class TensorLTScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x < scalar) 1.0 else 0.0;
}
  
case class TensorGTScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x > scalar) 1.0 else 0.0;
}
  
case class TensorLTEScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x <= scalar) 1.0 else 0.0;
}
  
case class TensorGTEScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x >= scalar) 1.0 else 0.0;
}

case class TensorEqScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x == scalar) 1.0 else 0.0;
}
  
case class TensorNeScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != scalar) 1.0 else 0.0;
}
  
case class TensorOrScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != 0.0 || scalar != 0) 1.0 else 0.0;
}
  
case class TensorAndScalar[I,+Value<:Tensor[I]](override val tensor : TensorOp[I,Value], scalar : Double) extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != 0.0 && scalar != 0) 1.0 else 0.0;
}

case class TensorPlusTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](override val tensor : TensorOp[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :+= tensorB.value;
    rv;
  }
}
  
case class TensorMinusTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](override val tensor : TensorOp[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :-= tensorB.value;
    rv;
  }
}
  
case class TensorMultTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](override val tensor : TensorOp[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :*= tensorB.value;
    rv;
  }
}
  
case class TensorDivTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](override val tensor : TensorOp[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :/= tensorB.value;
    rv;
  }
}
 
case class TensorPowTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](override val tensor : TensorOp[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :^= tensorB.value;
    rv;
  }
}

case class TensorLTTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a < b) 1.0 else 0.0;
}
  
case class TensorGTTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a > b) 1.0 else 0.0;
}
  
case class TensorLTETensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a <= b) 1.0 else 0.0;
}
  
case class TensorGTETensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a >= b) 1.0 else 0.0;
}
  
case class TensorEqTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a == b) 1.0 else 0.0;
}
  
case class TensorNeTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != b) 1.0 else 0.0;
}
  
case class TensorAndTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 && b != 0) 1.0 else 0.0;
}
  
case class TensorOrTensor[I,+V1<:Tensor[I],+V2<:Tensor[I]](t1 : TensorOp[I,V1], t2 : TensorOp[I,V2]) extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 || b != 0) 1.0 else 0.0;
}


//
// Vector operations
//

/** Extra operators for Tensor1 implementations.  A column. */
trait Tensor1Op[I,+Value<:Tensor1[I]] extends TensorOp[I,Value] {
  /** Transposes this matrix. */
  def t : RowTensor1Op[I,Value] = Tensor1ToRow(this);
    
  override def unary_- : Tensor1Op[I,Value] = this match {
    case Tensor1Negation(n) => n.asInstanceOf[Tensor1Op[I,Value]];
    case _ => Tensor1Negation(this);
  }
    
  /** Vector-vector multiplication. */
  def * (op : RowTensor1Op[I,Tensor1[I]]) =
    VectorOuterMultVector[I,Tensor2[I,I]](this, op);
  
  //
  // type-narrowing
  //
  
  // typed scalar operators
  override def  + (s : Double) = Tensor1PlusScalar(this, s);
  override def  - (s : Double) = Tensor1PlusScalar(this, -s);
  override def  * (s : Double) : Tensor1Op[I,Value] = Tensor1MultScalar(this, s);
  override def  / (s : Double) : Tensor1Op[I,Value] = Tensor1MultScalar(this, 1.0 / s);
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
  override def :+  (op : TensorOp[I,Tensor[I]]) = Tensor1PlusTensor(this, op);
  override def :-  (op : TensorOp[I,Tensor[I]]) = Tensor1MinusTensor(this, op);
  override def :*  (op : TensorOp[I,Tensor[I]]) = Tensor1MultTensor(this, op);
  override def :/  (op : TensorOp[I,Tensor[I]]) = Tensor1DivTensor(this, op);
  override def :^  (op : TensorOp[I,Tensor[I]]) = Tensor1PowTensor(this, op);
  override def :<  (op : TensorOp[I,Tensor[I]]) = Tensor1LTTensor(this, op);
  override def :>  (op : TensorOp[I,Tensor[I]]) = Tensor1GTTensor(this, op);
  override def :<= (op : TensorOp[I,Tensor[I]]) = Tensor1LTETensor(this, op);
  override def :>= (op : TensorOp[I,Tensor[I]]) = Tensor1GTETensor(this, op);
  override def :== (op : TensorOp[I,Tensor[I]]) = Tensor1EqTensor(this, op);
  override def :!= (op : TensorOp[I,Tensor[I]]) = Tensor1NeTensor(this, op);
  override def :&& (op : TensorOp[I,Tensor[I]]) = Tensor1AndTensor(this, op);
  override def :|| (op : TensorOp[I,Tensor[I]]) = Tensor1OrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[I,Tensor[I]]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[I,Tensor[I]]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[I,Tensor[I]]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[I,Tensor[I]]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[I,Tensor[I]]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[I,Tensor[I]]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[I,Tensor[I]]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[I,Tensor[I]]) = this :|| op;
}

case class Tensor1Identity[I,+Value<:Tensor1[I]](override val tensor : Value) extends TensorIdentity[I,Value](tensor) with Tensor1Op[I,Value] {
  override def value : Value = tensor;
}

case class Tensor1ToCol[I,+Value<:Tensor1[I]](op : RowTensor1Op[I,Value]) extends Tensor1Op[I,Value] {
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : MergeableSet[J]) = op.create(d);
}
  
case class Tensor1Negation[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value]) extends TensorNegation[I,Value](tensor) with Tensor1Op[I,Value] {
  override def value = negated;
}

case class Tensor1PlusScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with Tensor1Op[I,Value] {
  override def  + (s : Double) = Tensor1PlusScalar(tensor, scalar + s);
  override def  - (s : Double) = Tensor1PlusScalar(tensor, scalar - s);
}

case class Tensor1MultScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with Tensor1Op[I,Value] {
  override def * (s : Double) = Tensor1MultScalar(tensor, scalar * s);
  override def / (s : Double) = Tensor1MultScalar(tensor, scalar / s);
}

case class Tensor1PowScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1LTScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1GTScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1LTEScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1GTEScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1AndScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1OrScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1EqScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1NeScalar[I,+Value<:Tensor1[I]](override val tensor : Tensor1Op[I,Value], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with Tensor1Op[I,Value] { }

case class Tensor1PlusTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorPlusTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1MinusTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorMinusTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1MultTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorMultTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1DivTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorDivTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1PowTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorPowTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1LTTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorLTTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1GTTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorGTTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1LTETensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorLTETensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1GTETensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorGTETensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1EqTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorEqTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1NeTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorNeTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1AndTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorAndTensor(tensor, tensorB) with Tensor1Op[I,V1] { }

case class Tensor1OrTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : Tensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorOrTensor(tensor, tensorB) with Tensor1Op[I,V1] { }



//
// Row Tensor1 operations
//

/** Extra operators for Tensor1 implementations as a row. */
trait RowTensor1Op[I,+Value<:Tensor1[I]] extends TensorOp[I,Value] {
  /** Transposes this matrix. */
  def t : Tensor1Op[I,Value] = Tensor1ToCol(this);
    
  override def unary_- : RowTensor1Op[I,Value] = this match {
    case RowTensor1Negation(n) => n.asInstanceOf[RowTensor1Op[I,Value]];
    case _ => RowTensor1Negation(this);
  }
    
  /** Vector-matrix inner multiplication. */
  def *[J] (op : MatrixOp[I,J,Tensor2[I,J]]) =
    VectorInnerMultMatrix[I,J,Tensor1,Tensor1[J]](this, op);
    
  /** Vector-vector inner multiplication. */
  def * (op : Tensor1Op[I,Tensor1[I]]) = this.value dot op.value;
  
  //
  // type-narrowing
  //
    
  // typed scalar operators
  override def  + (s : Double) = RowTensor1PlusScalar(this, s);
  override def  - (s : Double) = RowTensor1PlusScalar(this, -s);
  override def  * (s : Double) : RowTensor1Op[I,Value] = RowTensor1MultScalar(this, s);
  override def  / (s : Double) : RowTensor1Op[I,Value] = RowTensor1MultScalar(this, 1.0 / s);
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
  override def :+  (op : TensorOp[I,Tensor[I]]) = RowTensor1PlusTensor(this, op);
  override def :-  (op : TensorOp[I,Tensor[I]]) = RowTensor1MinusTensor(this, op);
  override def :*  (op : TensorOp[I,Tensor[I]]) = RowTensor1MultTensor(this, op);
  override def :/  (op : TensorOp[I,Tensor[I]]) = RowTensor1DivTensor(this, op);
  override def :^  (op : TensorOp[I,Tensor[I]]) = RowTensor1PowTensor(this, op);
  override def :<  (op : TensorOp[I,Tensor[I]]) = RowTensor1LTTensor(this, op);
  override def :>  (op : TensorOp[I,Tensor[I]]) = RowTensor1GTTensor(this, op);
  override def :<= (op : TensorOp[I,Tensor[I]]) = RowTensor1LTETensor(this, op);
  override def :>= (op : TensorOp[I,Tensor[I]]) = RowTensor1GTETensor(this, op);
  override def :== (op : TensorOp[I,Tensor[I]]) = RowTensor1EqTensor(this, op);
  override def :!= (op : TensorOp[I,Tensor[I]]) = RowTensor1NeTensor(this, op);
  override def :&& (op : TensorOp[I,Tensor[I]]) = RowTensor1AndTensor(this, op);
  override def :|| (op : TensorOp[I,Tensor[I]]) = RowTensor1OrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[I,Tensor[I]]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[I,Tensor[I]]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[I,Tensor[I]]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[I,Tensor[I]]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[I,Tensor[I]]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[I,Tensor[I]]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[I,Tensor[I]]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[I,Tensor[I]]) = this :|| op;
}
 
case class Tensor1ToRow[I,+Value<:Tensor1[I]](op : Tensor1Op[I,Value]) extends RowTensor1Op[I,Value] {
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : MergeableSet[J]) = op.create(d);
}

case class RowTensor1Negation[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value]) extends TensorNegation[I,Value](tensor) with RowTensor1Op[I,Value] {
  override def value = negated;
}

case class RowTensor1PlusScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with RowTensor1Op[I,Value] {
  override def  + (s : Double) = RowTensor1PlusScalar(tensor, scalar + s);
  override def  - (s : Double) = RowTensor1PlusScalar(tensor, scalar - s);
}

case class RowTensor1MultScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with RowTensor1Op[I,Value] {
  override def * (s : Double) = RowTensor1MultScalar(tensor, scalar * s);
  override def / (s : Double) = RowTensor1MultScalar(tensor, scalar / s);
}

case class RowTensor1PowScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1LTScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1GTScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1LTEScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1GTEScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1AndScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1OrScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1EqScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1NeScalar[I,+Value<:Tensor1[I]](override val tensor : RowTensor1Op[I,Value], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with RowTensor1Op[I,Value] { }

case class RowTensor1PlusTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorPlusTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1MinusTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorMinusTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1MultTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorMultTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1DivTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorDivTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1PowTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorPowTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1LTTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorLTTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1GTTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorGTTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1LTETensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorLTETensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1GTETensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorGTETensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1EqTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorEqTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1NeTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorNeTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1AndTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorAndTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }

case class RowTensor1OrTensor[I,+V1<:Tensor1[I],+V2<:Tensor[I]](override val tensor : RowTensor1Op[I,V1], override val tensorB : TensorOp[I,V2]) extends TensorOrTensor(tensor, tensorB) with RowTensor1Op[I,V1] { }


//
// Vector operations
//

trait VectorOp[+Value<:Vector] extends Tensor1Op[Int,Value] {
  /** Vector-vector multiplication. */
  def * (op : RowVectorOp[Vector]) =
    VectorOuterMultVector[Int,Matrix](this, op);
  
  //
  // type-narrowing
  //
  
  // typed scalar operators
  override def  + (s : Double) = VectorPlusScalar(this, s);
  override def  - (s : Double) = VectorPlusScalar(this, -s);
  override def  * (s : Double) : VectorOp[Value] = VectorMultScalar(this, s);
  override def  / (s : Double) : VectorOp[Value] = VectorMultScalar(this, 1.0 / s);
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
  override def :+  (op : TensorOp[Int,Tensor[Int]]) = VectorPlusTensor(this, op);
  override def :-  (op : TensorOp[Int,Tensor[Int]]) = VectorMinusTensor(this, op);
  override def :*  (op : TensorOp[Int,Tensor[Int]]) = VectorMultTensor(this, op);
  override def :/  (op : TensorOp[Int,Tensor[Int]]) = VectorDivTensor(this, op);
  override def :^  (op : TensorOp[Int,Tensor[Int]]) = VectorPowTensor(this, op);
  override def :<  (op : TensorOp[Int,Tensor[Int]]) = VectorLTTensor(this, op);
  override def :>  (op : TensorOp[Int,Tensor[Int]]) = VectorGTTensor(this, op);
  override def :<= (op : TensorOp[Int,Tensor[Int]]) = VectorLTETensor(this, op);
  override def :>= (op : TensorOp[Int,Tensor[Int]]) = VectorGTETensor(this, op);
  override def :== (op : TensorOp[Int,Tensor[Int]]) = VectorEqTensor(this, op);
  override def :!= (op : TensorOp[Int,Tensor[Int]]) = VectorNeTensor(this, op);
  override def :&& (op : TensorOp[Int,Tensor[Int]]) = VectorAndTensor(this, op);
  override def :|| (op : TensorOp[Int,Tensor[Int]]) = VectorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[Int,Tensor[Int]]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[Int,Tensor[Int]]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[Int,Tensor[Int]]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[Int,Tensor[Int]]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[Int,Tensor[Int]]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[Int,Tensor[Int]]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[Int,Tensor[Int]]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[Int,Tensor[Int]]) = this :|| op;
}

case class VectorIdentity[+Value<:Vector](override val tensor : Value) extends Tensor1Identity[Int,Value](tensor) with VectorOp[Value] {
  override def value : Value = tensor;
}

case class VectorToCol[+Value<:Vector](op : RowVectorOp[Value]) extends VectorOp[Value] {
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : MergeableSet[J]) = op.create(d);
}
  
case class VectorNegation[+Value<:Vector](override val tensor : VectorOp[Value]) extends Tensor1Negation[Int,Value](tensor) with VectorOp[Value] {
  override def value = negated.asInstanceOf[Value];
}

case class VectorPlusScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1PlusScalar(tensor, scalar) with VectorOp[Value] {
  override def  + (s : Double) = VectorPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = VectorPlusScalar(tensor, scalar - s);
}

case class VectorMultScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1MultScalar(tensor, scalar) with VectorOp[Value] {
  override def * (s : Double) = VectorMultScalar(tensor, scalar * s);
  override def / (s : Double) = VectorMultScalar(tensor, scalar / s);
}

case class VectorPowScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1PowScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorLTScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1LTScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorGTScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1GTScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorLTEScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1LTEScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorGTEScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1GTEScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorAndScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1AndScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorOrScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1OrScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorEqScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1EqScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorNeScalar[+Value<:Vector](override val tensor : VectorOp[Value], override val scalar : Double) extends Tensor1NeScalar(tensor, scalar) with VectorOp[Value] {}

case class VectorPlusTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1PlusTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorMinusTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1MinusTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorMultTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1MultTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorDivTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1DivTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorPowTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1PowTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorLTTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1LTTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorGTTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1GTTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorLTETensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1LTETensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorGTETensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1GTETensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorEqTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1EqTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorNeTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1NeTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorAndTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1AndTensor(tensor, tensorB) with VectorOp[V1] {}

case class VectorOrTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : VectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends Tensor1OrTensor(tensor, tensorB) with VectorOp[V1] {}



//
// Row Vector operations
//

/** Extra operators for Tensor1 implementations as a row. */
trait RowVectorOp[+Value<:Vector] extends RowTensor1Op[Int,Value] {
  /** Transposes this matrix. */
  override def t : VectorOp[Value] = VectorToCol(this);
    
  override def unary_- : RowVectorOp[Value] = this match {
    case RowVectorNegation(n) => n;
    case _ => RowVectorNegation(this);
  }
    
  /** Vector-matrix inner multiplication. */
  override def *[J] (op : MatrixOp[Int,J,Tensor2[Int,J]]) =
    VectorInnerMultMatrix[Int,J,Tensor1,Tensor1[J]](this, op);
  
  /** Vector-vector inner multiplication. */
  def * (op : VectorOp[Vector]) = this.value dot op.value;
  
  //
  // type-narrowing
  //
    
  // typed scalar operators
  override def  + (s : Double) = RowVectorPlusScalar(this, s);
  override def  - (s : Double) = RowVectorPlusScalar(this, -s);
  override def  * (s : Double) : RowVectorOp[Value] = RowVectorMultScalar(this, s);
  override def  / (s : Double) : RowVectorOp[Value] = RowVectorMultScalar(this, 1.0 / s);
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
  override def :+  (op : TensorOp[Int,Tensor[Int]]) = RowVectorPlusTensor(this, op);
  override def :-  (op : TensorOp[Int,Tensor[Int]]) = RowVectorMinusTensor(this, op);
  override def :*  (op : TensorOp[Int,Tensor[Int]]) = RowVectorMultTensor(this, op);
  override def :/  (op : TensorOp[Int,Tensor[Int]]) = RowVectorDivTensor(this, op);
  override def :^  (op : TensorOp[Int,Tensor[Int]]) = RowVectorPowTensor(this, op);
  override def :<  (op : TensorOp[Int,Tensor[Int]]) = RowVectorLTTensor(this, op);
  override def :>  (op : TensorOp[Int,Tensor[Int]]) = RowVectorGTTensor(this, op);
  override def :<= (op : TensorOp[Int,Tensor[Int]]) = RowVectorLTETensor(this, op);
  override def :>= (op : TensorOp[Int,Tensor[Int]]) = RowVectorGTETensor(this, op);
  override def :== (op : TensorOp[Int,Tensor[Int]]) = RowVectorEqTensor(this, op);
  override def :!= (op : TensorOp[Int,Tensor[Int]]) = RowVectorNeTensor(this, op);
  override def :&& (op : TensorOp[Int,Tensor[Int]]) = RowVectorAndTensor(this, op);
  override def :|| (op : TensorOp[Int,Tensor[Int]]) = RowVectorOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[Int,Tensor[Int]]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[Int,Tensor[Int]]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[Int,Tensor[Int]]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[Int,Tensor[Int]]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[Int,Tensor[Int]]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[Int,Tensor[Int]]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[Int,Tensor[Int]]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[Int,Tensor[Int]]) = this :|| op;
}

case class VectorToRow[+Value<:Vector](op : VectorOp[Value]) extends RowVectorOp[Value] {
  override def domain = op.domain;
  override def value = op.value;
  override def working = op.working;
  override def create[J](d : MergeableSet[J]) = op.create(d);
}

case class RowVectorNegation[+Value<:Vector](override val tensor : RowVectorOp[Value]) extends RowTensor1Negation[Int,Value](tensor) with RowVectorOp[Value] {}

case class RowVectorPlusScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1PlusScalar(tensor, scalar) with RowVectorOp[Value] {
  override def  + (s : Double) = RowVectorPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = RowVectorPlusScalar(tensor, scalar - s);
}

case class RowVectorMultScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1MultScalar(tensor, scalar) with RowVectorOp[Value] {
  override def * (s : Double) = RowVectorMultScalar(tensor, scalar * s);
  override def / (s : Double) = RowVectorMultScalar(tensor, scalar / s);
}

case class RowVectorPowScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1PowScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorLTScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1LTScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorGTScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1GTScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorLTEScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1LTEScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorGTEScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1GTEScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorAndScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1AndScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorOrScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1OrScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorEqScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1EqScalar(tensor, scalar) with RowVectorOp[Value] {};

case class RowVectorNeScalar[+Value<:Vector](override val tensor : RowVectorOp[Value], override val scalar : Double) extends RowTensor1NeScalar(tensor, scalar) with RowVectorOp[Value] {}

case class RowVectorPlusTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1PlusTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorMinusTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1MinusTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorMultTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1MultTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorDivTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1DivTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorPowTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1PowTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorLTTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1LTTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorGTTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1GTTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorLTETensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1LTETensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorGTETensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1GTETensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorEqTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1EqTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorNeTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1NeTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorAndTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1AndTensor(tensor, tensorB) with RowVectorOp[V1] {}

case class RowVectorOrTensor[+V1<:Vector,V2<:Tensor[Int]](override val tensor : RowVectorOp[V1], override val tensorB : TensorOp[Int,V2]) extends RowTensor1OrTensor(tensor, tensorB) with RowVectorOp[V1] {}



//
// Matrix operations
//

/** Extra operators for Tensor2 implementations. */
trait MatrixOp[I1,I2,+Value<:Tensor2[I1,I2]] extends TensorOp[(I1,I2),Value] {
  override def domain : ProductSet[I1,I2];
  
  /** Transposes this tensor. */
  def t = MatrixTranspose[I1,I2,Tensor2[I2,I1]](this);
  
  override def unary_- : MatrixOp[I1,I2,Value] = this match {
    case MatrixNegation(n) => n.asInstanceOf[MatrixOp[I1,I2,Value]];
    case _ => MatrixNegation(this);
  }
  
  /** Matrix-matrix multiplication. */
  def *[J] (op : MatrixOp[I2,J,Tensor2[I2,J]]) =
    MatrixInnerMultMatrix[I1,I2,J,Tensor2[I1,J]](this, op);
  
  /** Matrix-vector multiplication. */
  def * (op : Tensor1Op[I2,Tensor1[I2]]) =
    MatrixInnerMultVector[I1,I2,Tensor1,Tensor1[I1]](this, op);
  
  /** Matrix solve. */
  def \[J] (op : MatrixOp[I1,J,Tensor2[I1,J]]) =
    MatrixSolveMatrix[I1,I2,J,Tensor2[I2,J]](this, op);
  
  /** Matrix-vector solve. */
  def \ (op : Tensor1Op[I1,Tensor1[I1]]) =
    MatrixSolveVector[I1,I2,Tensor1[I2]](this, op);
  
  //
  // type-narrowing
  //
  
  // typed scalar operators
  override def  + (s : Double) = MatrixPlusScalar(this, s);
  override def  - (s : Double) = MatrixPlusScalar(this, -s);
  override def  * (s : Double) : MatrixOp[I1,I2,Value] = MatrixMultScalar(this, s);
  override def  / (s : Double) : MatrixOp[I1,I2,Value] = MatrixMultScalar(this, 1.0 / s);
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
  override def :+  (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixPlusTensor(this, op);
  override def :-  (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixMinusTensor(this, op);
  override def :*  (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixMultTensor(this, op);
  override def :/  (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixDivTensor(this, op);
  override def :^  (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixPowTensor(this, op);
  override def :<  (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixLTTensor(this, op);
  override def :>  (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixGTTensor(this, op);
  override def :<= (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixLTETensor(this, op);
  override def :>= (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixGTETensor(this, op);
  override def :== (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixEqTensor(this, op);
  override def :!= (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixNeTensor(this, op);
  override def :&& (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixAndTensor(this, op);
  override def :|| (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = MatrixOrTensor(this, op);
  
  /** Fixed alias for :+ */
  override def + (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :+ op;
  /** Fixed alias for :- */
  override def - (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :- op;
  /** Fixed alias for :< */
  override def < (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :< op;
  /** Fixed alias for :> */
  override def > (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :> op;
  /** Fixed alias for :<= */
  override def <= (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :<= op;
  /** Fixed alias for :>= */
  override def >= (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :>= op;
  /** Fixed alias for :&& */
  override def && (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :&& op;
  /** Fixed alias for :|| */
  override def || (op : TensorOp[(I1,I2),Tensor[(I1,I2)]]) = this :|| op;
}

case class MatrixIdentity[I,J,+Value<:Tensor2[I,J]](override val tensor : Value) extends TensorIdentity[(I,J),Value](tensor) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
  override def value : Value = tensor;
}
  
case class MatrixNegation[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value]) extends TensorNegation[(I,J),Value](tensor) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
  override def value = negated;
}
  
case class MatrixTranspose[A,B,+Value<:Tensor2[B,A]](op : MatrixOp[A,B,Tensor2[A,B]]) extends MatrixOp[B,A,Value] {
  override lazy val domain = op.domain.transpose;
  override lazy val value : Value = op.value.transpose.asInstanceOf[Value];
  override def create[J](d : MergeableSet[J]) = op.create(d);
}
  
case class MatrixInnerMultMatrix[A1,INNER,B2,+Value<:Tensor2[A1,B2]](t1 : MatrixOp[A1,INNER,Tensor2[A1,INNER]], t2 : MatrixOp[INNER,B2,Tensor2[INNER,B2]]) extends MatrixOp[A1,B2,Value] {
  if (t1.domain._2 != t2.domain._1) throw new Predef.IllegalArgumentException;
    
  override lazy val domain = ProductSet(t1.domain._1, t2.domain._2);
  override lazy val value : Value = {
    val innerDomain = t1.domain._2;
    val t1v = t1.value;
    val t2v = t2.value;
    val rv = create(domain).asInstanceOf[Value];
    for (i <- domain._1; j <- domain._2) {
      rv(i,j) = t1v.getRow(i) dot t2v.getCol(j);
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = t1.create(d);
}
 
case class MatrixInnerMultVector[I,J,+ValueType[X]<:Tensor1[X],Value<:ValueType[I]](matrix : MatrixOp[I,J,Tensor2[I,J]], vector : Tensor1Op[J,ValueType[J]]) extends Tensor1Op[I,Value] {
  if (matrix.domain._2 != vector.domain) throw new DomainException;
    
  override def domain = matrix.domain._1;
  override lazy val value : Value = {
    val mv = matrix.value;
    val vv = vector.value;
    val rv = create(domain).asInstanceOf[Value];
    for (i <- domain) {
      rv(i) = mv.getRow(i) dot vv;
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = vector.create(d);
}
  
case class VectorInnerMultMatrix[I,J,+ValueType[A]<:Tensor1[A],Value<:ValueType[J]](vector : RowTensor1Op[I,ValueType[I]], matrix : MatrixOp[I,J,Tensor2[I,J]]) extends RowTensor1Op[J,Value] {
  if (vector.domain != matrix.domain._1) throw new DomainException;
    
  override def domain = matrix.domain._2;
  override lazy val value : Value = {
    val vv = vector.value;
    val mv = matrix.value;
    val rv = vv.create(domain).asInstanceOf[Value];
    for (j <- domain) {
      rv(j) = vv dot mv.getCol(j);
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = vector.create(d);
}

case class VectorOuterMultVector[I,+Value<:Tensor2[I,I]](v1 : Tensor1Op[I,Tensor1[I]], v2 : RowTensor1Op[I,Tensor1[I]]) extends MatrixOp[I,I,Value] {
  if (v1.domain != v2.domain) throw new DomainException;
    
  override def domain = ProductSet(v1.domain,v1.domain);
  override lazy val value : Value = {
    val v1v = v1.value;
    val v2v = v2.value;
    val rv = v1v.create(domain).asInstanceOf[Value];
    for (i <- v1.domain; j <- v1.domain) {
      rv(i,j) = v1v(i) * v2v(j);
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = v1.create(d);
}
  
case class MatrixSolveMatrix[K,I,J,+Value<:Tensor2[I,J]](m1 : MatrixOp[K,I,Tensor2[K,I]], m2 : MatrixOp[K,J,Tensor2[K,J]]) extends MatrixOp[I,J,Value] {
  if (m1.domain._1 != m2.domain._1) throw new DomainException;
    
  override def create[J](d : MergeableSet[J]) = m1.create(d);
  override def domain = ProductSet(m1.domain._2, m2.domain._2);
  override lazy val value : Value = {
    val X = m1.create(domain).asInstanceOf[Value];
    if (!X.isInstanceOf[scalala.tensor.MatrixMatrixSolver[_,_]]) {
      throw new UnsupportedOperationException("Type "+X.getClass+" does not support matrix solving");
    }
    X := this;
    X;
  }
}
  
case class MatrixSolveVector[I,J,+Value<:Tensor1[J]](m : MatrixOp[I,J,Tensor2[I,J]], v : Tensor1Op[I,Tensor1[I]]) extends Tensor1Op[J,Value] {
  if (m.domain._1 != v.domain) throw new DomainException;
  
  override def create[J](d : MergeableSet[J]) = v.create(d);
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

case class MatrixPlusScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorPlusScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
  override def  + (s : Double) = MatrixPlusScalar(tensor, scalar + s);
  override def  - (s : Double) = MatrixPlusScalar(tensor, scalar - s);
};

case class MatrixMultScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorMultScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
  override def * (s : Double) = MatrixMultScalar(tensor, scalar * s);
  override def / (s : Double) = MatrixMultScalar(tensor, scalar / s);
}

case class MatrixPowScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorPowScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixLTScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorLTScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixGTScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorGTScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixLTEScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorLTEScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixGTEScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorGTEScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixAndScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorAndScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixOrScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorOrScalar(tensor, scalar) with MatrixOp[I,J,Value] { 
  override def domain = tensor.domain;
}

case class MatrixEqScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorEqScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixNeScalar[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val scalar : Double) extends TensorNeScalar(tensor, scalar) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixPlusTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorPlusTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixMinusTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorMinusTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixMultTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorMultTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixDivTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorDivTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixPowTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorPowTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixLTTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorLTTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixGTTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorGTTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixLTETensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorLTETensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixGTETensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorGTETensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixEqTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorEqTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixNeTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorNeTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixAndTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorAndTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}

case class MatrixOrTensor[I,J,+Value<:Tensor2[I,J]](override val tensor : MatrixOp[I,J,Value], override val tensorB : TensorOp[(I,J),Tensor[(I,J)]]) extends TensorOrTensor(tensor, tensorB) with MatrixOp[I,J,Value] {
  override def domain = tensor.domain;
}


package sparse {
  import scalala.tensor.sparse._;
  
  case class SparseBinaryVectorIdentity(vector : SparseBinaryVector) extends VectorOp[SparseVector] {
    type Value = SparseVector;
    override def domain = vector.domain;
    lazy val _value = vector.toSparseVector
    override def value : Value = _value;
    override def working : Value = vector.toSparseVector;
    override def create[J](d : MergeableSet[J]) = SparseVector.create(d);
    
    override def t = SparseBinaryRowVectorIdentity(vector);
  }
  
  case class SparseBinaryRowVectorIdentity(vector : SparseBinaryVector) extends RowVectorOp[SparseVector] {
    type Value = SparseVector;
    override def domain = vector.domain;
    lazy val _value = vector.toSparseVector;
    override def value : Value = _value;
    override def working : Value = vector.toSparseVector;
    override def create[J](d : MergeableSet[J]) = SparseVector.create(d);
    
    override def t = SparseBinaryVectorIdentity(vector);
    
    /** Vector-vector inner multiplication. */
    def * (op : VectorIdentity[Value]) = this.vector dot op.value;
  }
  
  case class SingletonBinaryVectorIdentity(vector : SingletonBinaryVector) extends VectorOp[SparseVector] {
    type Value = SparseVector;
    override def domain = vector.domain;
    lazy val _value = vector.toSparseVector
    override def value : Value = _value;
    override def working : Value = vector.toSparseVector;
    override def create[J](d : MergeableSet[J]) = SparseVector.create(d);
    
    override def t = SingletonBinaryRowVectorIdentity(vector);
  }
  
  case class SingletonBinaryRowVectorIdentity(vector : SingletonBinaryVector) extends RowVectorOp[SparseVector] {
    type Value = SparseVector;
    override def domain = vector.domain;
    lazy val _value = vector.toSparseVector;
    override def value : Value = _value;
    override def working : Value = vector.toSparseVector;
    override def create[J](d : MergeableSet[J]) = SparseVector.create(d);
    
    override def t = SingletonBinaryVectorIdentity(vector);
    
    /** Vector-vector inner multiplication. */
    def * (op : VectorIdentity[Value]) = this.vector dot op.value;
  }
}

package dense {
  import scalala.tensor.dense._;
  
  trait DenseMatrixOp extends MatrixOp[Int,Int,DenseMatrix] {
    def rows = domain._1.asInstanceOf[IntSpanSet].end;
    def cols = domain._2.asInstanceOf[IntSpanSet].end;

    //override def t = DenseMatrixTranspose(this);
    
    override def create[J](domain : MergeableSet[J]) = 
      new DenseMatrix(0,0).create(domain);
  }
  
  case class DenseMatrixNegation(override val tensor : DenseMatrixOp) extends MatrixNegation[Int,Int,DenseMatrix](tensor) with DenseMatrixOp {}
  case class DenseMatrixTranspose(override val op : DenseMatrixOp) extends MatrixTranspose[Int,Int,DenseMatrix](op) with DenseMatrixOp {}
  
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
