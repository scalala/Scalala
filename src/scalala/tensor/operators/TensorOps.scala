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


/** Implicits for TensorOp support. */
trait TensorOps {
////  implicit def iTensorToTensorOp[I,V<:Tensor[I]](tensor : V) =
////    TensorIdentity[I,Tensor[I],V,Any](tensor);
//  implicit def iTensorToTensorOp[I](tensor : Tensor[I]) =
//    TensorIdentity[I,Tensor[I],Tensor[I],Any](tensor);
//
////  implicit def iTensorToRichTensorOp[I,V<:Tensor[I]](tensor : V) =
////    new RichTensorOp[I,Tensor[I],V,Any](tensor);
//  implicit def iTensorToRichTensorOp[I](tensor : Tensor[I]) =
//    new RichTensorOp[I,Tensor[I],Tensor[I],Any](tensor);
  
//  implicit def iScalarToRichScalarTensorOp(s : Double) =
//    new RichScalarTensorOp(s);
  
//  implicit def iTensorOpToTensor[I,Base<:Tensor[I],V<:Base]
//   (x : TensorOp[I,Base,V,_]) : V =
//    x.value;
  
}

/** Singleton instance of TensorOps trait. */
object TensorOps extends TensorOps;


object TensorShapes {
  /** The shape of a tensor. */
  abstract sealed class TensorShape;
  
  /** A shape that is assignable. */
  abstract sealed class PublicShape extends TensorShape;
  
  /** A shape that is unassignable, like row. */
  abstract sealed class PrivateShape extends TensorShape;
  
  /** Singly indexed shape. */
  final abstract class AnyShape extends PublicShape;
  
  /** Matrix shape */
  final abstract class Shape2[I,J] extends PublicShape;
  
  /** Column vector shape. */
  final abstract class Shape1Col extends PublicShape;
  
  /** Row vector shape. */
  final abstract class Shape1Row extends PrivateShape;
}

import TensorShapes._;

/**
 * A TensorOp represents a possibly not-yet-computed tensor
 * value.  Bound is a compatibility bound that limits the
 * type of tensors this tensor may be composed with (e.g.
 * Vectors only with other Vectors); Value is the return value
 * of this particular operation; and Shape is an extra parameter
 * to keep more shape information (needed for row vs column
 * tensors.)
 * 
 * @author dramage
 */
trait TensorOp[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape] {
  /** Returns the domain of the output of this Tensor. */
  def domain : MergeableSet[I];
  
  /**
   * Returns the value of this operator in a Vector that might
   * be visible outside of the evaluator, i.e. that its value
   * should not be mutated.
   */
  def value : Value;
  
  protected var workingUsed = false;
  /**
   * Returns the value of this operator in a vector than can safely
   * be over-written.  Defaults to this.value but is override in
   * TensorIdentity and related operators to be this.value.copy.
   * This method throws an exception if it is called more than once --
   * i.e. if this op's temp space is to be relied upon multiple times.
   */
  def working : Value = {
    if (workingUsed) {
      throw new IllegalAccessException("TensorOp's working value "+
        "accessed more than once: are you keeping direct "+
        "references to TensorOp objects?");
    }
    workingUsed = true;
    value;
  }
    
  /** Creates a new tensor for the requested domain type. */
  def create[J](d : MergeableSet[J]) : Tensor[J];
}

/**
 * Operations applicable to any TensorOp via tensor-scalar and
 * tensor-tensor arithmetic.  Types are left-associative -- the
 * value of say a DenseVector + Vector will be a DenseVector,
 * but a Vector + DenseVector will be a Vector.
 * 
 * @author dramage
 */
class RichTensorOp[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(base : TensorOp[I,Bound,Value,Shape]) {
  
  /** Unary minus returns a tensor negation. */
  def unary_- = TensorNegation(base);
  
  //
  // scalar operators
  //
  
  def  + (s : Double) = TensorPlusScalar(base, s);
  def  - (s : Double) = TensorPlusScalar(base, -s);
  def  * (s : Double) = TensorMultScalar(base, s);
  def  / (s : Double) = TensorMultScalar(base, 1.0 / s);
  def :^ (s : Double) = TensorPowScalar(base, s);
  def  < (s : Double) = TensorLTScalar(base, s);
  def  > (s : Double) = TensorGTScalar(base, s);
  def <= (s : Double) = TensorLTEScalar(base, s);
  def >= (s : Double) = TensorGTEScalar(base, s);
  def && (s : Double) = TensorAndScalar(base, s);
  def || (s : Double) = TensorOrScalar(base, s);
  
  /** Colon prefix is required to avoid conflation with .equals */
  def :== (s : Double) = TensorEqScalar(base, s);
  
  /** Colon prefix is required to avoid conflation with .equals */
  def :!= (s : Double) = TensorNeScalar(base, s);
  
  //
  // tensor operators
  //
    
  def :+ [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorPlusTensor(base, op);
  
  def :- [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorMinusTensor(base, op);
  
  def :*  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorMultTensor(base, op);
  
  def :/  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorDivTensor(base, op);
  
  def :^  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorPowTensor(base, op);
  
  def :<  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorLTTensor(base, op);
  
  def :>  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorGTTensor(base, op);
  
  def :<= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorLTETensor(base, op);
  
  def :>= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorGTETensor(base, op);
  
  def :== [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorEqTensor(base, op);
  
  def :!= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorNeTensor(base, op);
  
  def :&& [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorAndTensor(base, op);
  
  def :|| [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    TensorOrTensor(base, op);
  
  /** Fixed alias for :+ */
  final def + [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :+ op;
  
  /** Fixed alias for :- */
  final def - [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :- op;
  
  /** Fixed alias for :< */
  final def < [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :< op;
  
  /** Fixed alias for :> */
  final def > [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :> op;
  
  /** Fixed alias for :<= */
  final def <= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :<= op;
  
  /** Fixed alias for :>= */
  final def >= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :>= op;
  
  /** Fixed alias for :&& */
  final def && [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :&& op;
  
  /** Fixed alias for :|| */
  final def || [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :|| op;
}

///**
// * A rich scalar extension with support for tensor operators.
// */
//class RichScalarTensorOp(s : Double) {
//  
//  def  + [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorPlusScalar(base, s);
//  
//  final def  + [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorPlusScalar(base, s);
//  }
//  
//  def  - [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorPlusScalar(ops.mkTensorNegation(base), s);
//  
//  final def  - [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorPlusScalar(ops.mkTensorNegation(base), s);
//  }
//  
//  def  * [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorMultScalar(base, s);
//  
//  final def  * [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorMultScalar(base, s);
//  }
//  
//  def  / [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkScalarDivTensor(s, base);
//  
//  final def  / [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkScalarDivTensor(s, base);
//  }
//  
//  def :^ [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkScalarPowTensor(s, base);
//  
//  final def :^ [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkScalarPowTensor(s, base);
//  }
//  
//  def  < [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorGTScalar(base, s);
//  
//  final def  < [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorGTScalar(base, s);
//  }
//  
//  def  > [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorLTScalar(base, s);
//  
//  final def  > [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorLTScalar(base, s);
//  }
//  
//  def <= [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorGTEScalar(base, s);
//  
//  final def  <= [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorGTEScalar(base, s);
//  }
//  
//  def >= [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorLTEScalar(base, s);
//  
//  final def  >= [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorLTEScalar(base, s);
//  }
//  
//  def && [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorAndScalar(base, s);
//  
//  final def  && [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorAndScalar(base, s);
//  }
//  
//  def || [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorOrScalar(base, s);
//  
//  final def  || [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorOrScalar(base, s);
//  }
//  
//  /** Colon prefix is required to avoid conflation with .equals */
//  def :== [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorEqScalar(base, s);
//  
//  final def :== [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorEqScalar(base, s);
//  }
//  
//  /** Colon prefix is required to avoid conflation with .equals */
//  def :!= [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (base : TensorOp[I,Bound,Value,Shape])
//  (implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) =
//    ops.mkTensorNeScalar(base, s);
//  
//  final def :!= [I,Bound<:Tensor[I],Value<:Bound,Shape]
//  (t : Value)(implicit ops : TensorOpBuilderImpl[I,Bound,Shape]) = {
//    val base = ops.mkTensorIdentity(t);
//    ops.mkTensorNeScalar(base, s);
//  }
//}

/** A reference to an underlying tensor. */
case class TensorIdentity[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(tensor : Value) extends TensorOp[I,Bound,Value,Shape] {  
  override def domain = tensor.domain;
  override def value = tensor;
  override def working = tensor.copy.asInstanceOf[Value];
  override def create[J](d : MergeableSet[J]) = tensor.create(d);
}

/** A reference to an underlying TensorOp. */
abstract case class TensorReferenceOp[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(tensor : TensorOp[I,Bound,Value,Shape])
extends TensorOp[I,Bound,Value,Shape] {
  override def domain = tensor.domain;
  override def create[J](d : MergeableSet[J]) = tensor.create(d);
}
  
/** An operation applied to a tensor's values. */
abstract case class TensorFunctionOp[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape])
extends TensorReferenceOp[I,Bound,Value,Shape](tensor) {
  /** Function to apply to elements to compute new value. */
  def function(x : Double) : Double;
  
  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = function(rv.default);
    rv(rv.activeDomain) = function _;
    rv;
  }
}
  
/** An operator between two tensors defined on the same domain. */
abstract class TensorTensorOp[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,V1,Shape], val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorReferenceOp[I,Bound,V1,Shape](tensor) {
  if (tensor.domain != tensorB.domain) throw new DomainException("Tensors have incompatible domain");
}

/** An abstract operator between two tensors and a function of their values. */
abstract class TensorTensorFunctionOp[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensorA, tensorB) {
  /** Function of paired elements to compute new value. */
  def function(a : Double, b : Double) : Double;
    
  override lazy val value : V1 = {
    val rv = tensorA.working;
    val tb = tensorB.value;
    rv.default = function(rv.default, tb.default);
    for (e <- rv.activeDomain ++ tb.activeDomain) {
      rv(e) = function(rv(e),tb(e));
    }
    rv;
  }
}

/** The negation of a tensor. */
case class TensorNegation[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape])
extends TensorReferenceOp[I,Bound,Value,Shape](tensor) {
  override def value = {
    val rv = tensor.working;
    rv *= -1;
    rv;
  }
}

//object TensorNegation {
//  trait Optimizations[I,Value<:Tensor[I]] extends TensorNegation[I,Value] {
//    override def unary_- = tensor;
//  }
//}

/** Adds together to tensors. */
case class TensorPlusScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorReferenceOp(tensor) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv += scalar;
    rv;
  }
}

//object TensorPlusScalar {
//  trait Optimizations[I,Value<:Tensor[I]] extends TensorPlusScalar[I,Value] {
//    override def + (s : Double) = (tensor + (scalar + s));
//    override def - (s : Double) = (tensor + (scalar - s));
//  }
//}

/** Scales a tensor by a scalar: 2 * x. */
case class TensorMultScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorReferenceOp(tensor) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv *= scalar;
    rv;
  }
}

//object TensorMultScalar {
//  trait Optimizations[I,Bound<:Tensor[I],Value<:Bound,Shape] extends TensorMultScalar[I,Value] {
//    override def * (s : Double) = (tensor * (scalar * s));
//    override def / (s : Double) = (tensor * (scalar / s));
//  }
//}

/** Inverts a tensor and scales: 2 / x. */
case class ScalarDivTensor[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(scalar : Double, override val tensor : TensorOp[I,Bound,Value,Shape])
extends TensorReferenceOp(tensor) {
//  override def * (s : Double) = ScalarDivTensor(scalar * s, tensor);
//  override def / (s : Double) = ScalarDivTensor(scalar / s, tensor);
  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = scalar / rv.default;
    rv(rv.activeDomain) = ((x:Double) => scalar / x);
    rv;
  }
}

/** Raises a tensor to a power, element wise: x :^ 2 */
case class TensorPowScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorReferenceOp(tensor) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :^= scalar;
    rv;
  }
}

/**
 * Raises each element of a tensor as the exponent with scalar as
 * the base: 2 :^ x.
 */
case class ScalarPowTensor[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(scalar : Double, override val tensor : TensorOp[I,Bound,Value,Shape])
extends TensorReferenceOp(tensor) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = Math.pow(scalar, rv.default);
    rv(rv.activeDomain) = ((x:Double) => Math.pow(scalar, x));
    rv;
  }
}

/** Tensor less than a scalar: x < 2. */
case class TensorLTScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x < scalar) 1.0 else 0.0;
}

/** Tensor greater than a scalar: x > 2. */
case class TensorGTScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x > scalar) 1.0 else 0.0;
}

/** Tensor less than or equal to a scalar: x <= 2. */
case class TensorLTEScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x <= scalar) 1.0 else 0.0;
}

/** Tensor greater than or equal to a scalar: x >= 2. */
case class TensorGTEScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x >= scalar) 1.0 else 0.0;
}

/** Tensor equal to a scalar in each element: x :== 2. */
case class TensorEqScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x == scalar) 1.0 else 0.0;
}

/** Tensor not equal to a scalar in each element: x :!= 2. */
case class TensorNeScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != scalar) 1.0 else 0.0;
}

/** Either tensor or scalar is non-zero in each position: x :|| 0. */
case class TensorOrScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != 0.0 || scalar != 0) 1.0 else 0.0;
}

/** Both tensor and scalar are non-zero in each position: x :&& 1. */
case class TensorAndScalar[I,Bound<:Tensor[I],Value<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != 0.0 && scalar != 0) 1.0 else 0.0;
}

/** Tensor addition: x + y. */
case class TensorPlusTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :+= tensorB.value;
    rv;
  }
}

/** Tensor subtraction: x - y. */
case class TensorMinusTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :-= tensorB.value;
    rv;
  }
}

/** Tensor element-wise multiplication: x :* y. */
case class TensorMultTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :*= tensorB.value;
    rv;
  }
}

/** Tensor element-wise division: x :/ y. */
case class TensorDivTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :/= tensorB.value;
    rv;
  }
}

/** Tensor element-wise exponentiatoin: x :^ y. */
case class TensorPowTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :^= tensorB.value;
    rv;
  }
}

/** Tensor element-wise less than: x :< y. */
case class TensorLTTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a < b) 1.0 else 0.0;
}

/** Tensor element-wise greater than: x :> y. */
case class TensorGTTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a > b) 1.0 else 0.0;
}

/** Tensor element-wise less than or equal to: x :<= y. */
case class TensorLTETensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a <= b) 1.0 else 0.0;
}

/** Tensor element-wise greater than or equal to: x :>= y. */
case class TensorGTETensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a >= b) 1.0 else 0.0;
}

/** Tensor element-wise (strict) equality: x :== y. */
case class TensorEqTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a == b) 1.0 else 0.0;
}

/** Tensor element-wise (strict) non-equality: x :!= y. */
case class TensorNeTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != b) 1.0 else 0.0;
}

/** Tensor element-wise test for both non-zero: x :&& y. */
case class TensorAndTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 && b != 0) 1.0 else 0.0;
}

/** Tensor element-wise test for either non-zero: x :|| y. */
case class TensorOrTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape<:TensorShape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 || b != 0) 1.0 else 0.0;
}
