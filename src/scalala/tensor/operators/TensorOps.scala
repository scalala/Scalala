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

import scalala.collection.{MergeableSet, IntSpanSet, PartialMap, DomainException};
import scalala.tensor.{Tensor1, Tensor, Tensor2, Vector, Matrix};

object TensorShapes {
  /** The shape of a tensor. */
  abstract sealed class TensorShape;

  /** A shape that is assignable. */
  abstract sealed class PublicShape extends TensorShape;

  /** A shape that is unassignable, like row. */
  abstract sealed class PrivateShape extends TensorShape;

  /** Singly indexed shape. */
  sealed abstract class AnyShape extends PublicShape;
  type Shape2 = AnyShape;
  type Shape1Col = AnyShape;

  /** Row vector shape. */
  sealed abstract class Shape1Row extends PrivateShape;
}

import TensorShapes._;


/** Implicits for TensorOp support. */
trait TensorOps {
  implicit def tensorArith[I] = new TensorArith[I,Tensor[I],Tensor[I],AnyShape];
}

/** Singleton instance of TensorOps trait. */
object TensorOps extends TensorOps;

/**
* Creates a tensor that can be used as the result of multiplying
* these two tensors together.
*/
trait TensorProductBuilder[Value<:Tensor[_],-V2<:Tensor[_],+R<:Tensor[_],S1<:TensorShape,S2<:TensorShape,Shape<:TensorShape] {
  def create(v1: Value, v2: V2): R;
  def makeProduct(v1: TensorOp[Value,S1], v2: TensorOp[V2,S2]): TensorOp[R,Shape];
}

/**
 * A TensorOp represents a possibly not-yet-computed tensor
 * value.  Value is the return value
 * of this particular operation; and Shape is an extra parameter
 * to keep more shape information (needed for row vs column
 * tensors.)
 *
 * @author dramage
 */
trait TensorOp[+Value<:Tensor[_],Shape<:TensorShape] {
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

}

/**
 * Operations applicable to any TensorOp via tensor-scalar and
 * tensor-tensor arithmetic.  Types are left-associative -- the
 * value of say a DenseVector + Vector will be a DenseVector,
 * but a Vector + DenseVector will be a Vector.
 *
 * @author dramage
 */
class RichTensorOp[Value<:Tensor[_],Shape<:TensorShape]
(base : TensorOp[Value,Shape])
/*(implicit builder: TensorBuilder[Value])*/ {

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

  def :+ [V<:Tensor[_]] (op : TensorOp[V,Shape])
      (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.plus(base, op);

  def :- [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.minus(base, op);

  def :* [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.times(base, op);

  def :/ [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.div(base, op);

  def :^ [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.pow(base, op);

  def :< [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.lt(base, op);

  def :>  [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.gt(base, op);

  def :<= [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.lte(base, op);

  def :>= [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.gte(base, op);

  def :== [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.eq(base, op);

  def :!= [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.ne(base, op);

  def :&& [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.and(base, op);

  def :|| [V<:Tensor[_]] (op : TensorOp[V,Shape])
    (implicit ops: TensorArith[_,Value,V,Shape]):TensorOp[Value,Shape] =
    ops.or(base, op);

  /** Fixed alias for :+ */
  final def + [V<:Tensor[_]](op : TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :+ op;

  /** Fixed alias for :- */
  final def - [V<:Tensor[_]](op : TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :- op;

  /** Fixed alias for :&lt; */
  final def < [V<:Tensor[_]] (op: TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :< op;

  /** Fixed alias for :&gt; */
  final def > [V<:Tensor[_]] (op: TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :> op;

  /** Fixed alias for :&lt;= */
  final def <= [V<:Tensor[_]] (op: TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :<= op;

  /** Fixed alias for :&gt;= */
  final def >= [V<:Tensor[_]] (op: TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :>= op;

  /** Fixed alias for :&amp;&amp; */
  final def && [V<:Tensor[_]] (op: TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :&& op;

  /** Fixed alias for :|| */
  final def || [V<:Tensor[_]] (op: TensorOp[V,Shape])(implicit ops: TensorArith[_,Value,V,Shape]) = this :|| op;
}

/**
 * A rich scalar extension with support for tensor operators.
 */
class RichScalarTensorOp(s : Double) {

  def  + [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape]) =
    TensorPlusScalar(base, s);

  def  - [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape]) =
  TensorPlusScalar(TensorNegation(base), s);

  def  * [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorMultScalar(base, s);

  def  / [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    ScalarDivTensor(s, base);

  def :^ [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    ScalarPowTensor(s, base);

  def  < [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorGTScalar(base, s);

  def  > [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorLTScalar(base, s);

  def <= [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorGTEScalar(base, s);

  def >= [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorLTEScalar(base, s);

  def && [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorAndScalar(base, s);

  def || [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorOrScalar(base, s);

  /** Colon prefix is required to avoid conflation with .equals */
  def :== [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorEqScalar(base, s);

  /** Colon prefix is required to avoid conflation with .equals */
  def :!= [Value<:Tensor[_],Shape<:TensorShape]
  (base : TensorOp[Value,Shape])
   =
    TensorNeScalar(base, s);
}

/** Tensors should extends this to maintain their type in TensorOps. */
trait TensorSelfOp[I,+Value<:Tensor[I] with TensorSelfOp[I,Value,Shape],Shape<:TensorShape]
    extends TensorOp[Value,Shape] { this: Value =>
  override def value :Value= this;
  def like: Value;
  def copy: Value = {
    val r = like;
    r :+= (this:PartialMap[I,Double]);
    r;
  }
  override def working : Value = copy;
}

/** A reference to an underlying TensorOp. */
abstract class TensorReferenceOp[Value<:Tensor[_],Shape<:TensorShape]
(tensor : TensorOp[Value,Shape])
extends TensorOp[Value,Shape];

/** An operation applied to a tensor's values. */
abstract class TensorFunctionOp[Value<:Tensor[_],Shape<:TensorShape]
(tensor : TensorOp[Value,Shape])
extends TensorReferenceOp(tensor) {
  /** Function to apply to elements to compute new value. */
  def function(x : Double) : Double;

  override lazy val value : Value = {
    val rv = tensor.working;
    rv.default = function(rv.default);
    rv.transform(function _);
    rv;
  }
}

/** An operator between two tensors defined on the same domain. */
abstract class TensorTensorOp[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
  (tensor : TensorOp[Value,Shape], val tensorB : TensorOp[V2,Shape])
extends TensorReferenceOp[Value,Shape](tensor) {
}

/** An abstract operator between two tensors and a function of their values. */
abstract class TensorTensorFunctionOp[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(tensorA : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape])
extends TensorTensorOp[I,Value,V2,Shape](tensorA, tensorB) {
  /** Function of paired elements to compute new value. */
  def function(a : Double, b : Double) : Double;

  override lazy val value : Value = {
    val rv = tensorA.working;
    val tb = tensorB.value;
    rv.default = function(rv.default, tb.default);
    for (e <- rv.activeDomain ++ tb.activeDomain) {
      rv(e) = function(rv(e),tb(e));
    }
    rv;
  }
}

//object TensorNegation {
//  trait Optimizations[Value<:Tensor[_]] extends TensorNegation[Value] {
//    override def unary_- = tensor;
//  }
//}

/** Tensor addition: x + y. */
case class TensorPlusTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(val tensor : TensorOp[Value,Shape], override val tensorB : TensorOp[V2,Shape])
extends TensorTensorOp[I,Value,V2,Shape](tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :+= tensorB.value;
    rv;
  }
}

/** Tensor subtraction: x - y. */
case class TensorMinusTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(val tensor : TensorOp[Value,Shape], override val tensorB : TensorOp[V2,Shape])
extends TensorTensorOp[I,Value,V2,Shape](tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :-= tensorB.value;
    rv;
  }
}

/** Tensor element-wise multiplication: x :* y. */
case class TensorMultTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(val tensor : TensorOp[Value,Shape], override val tensorB : TensorOp[V2,Shape])
extends TensorTensorOp[I,Value,V2,Shape](tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :*= tensorB.value;
    rv;
  }
}

/** Tensor element-wise division: x :/ y. */
case class TensorDivTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(val tensor : TensorOp[Value,Shape], override val tensorB : TensorOp[V2,Shape])
extends TensorTensorOp[I,Value,V2,Shape](tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :/= tensorB.value;
    rv;
  }
}

/** Tensor element-wise exponentiatoin: x :^ y. */
case class TensorPowTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(val tensor : TensorOp[Value,Shape], override val tensorB : TensorOp[V2,Shape])
extends TensorTensorOp[I,Value,V2,Shape](tensor,tensorB) {
  override lazy val value : Value = {
    val rv = tensor.working;
    rv :^= tensorB.value;
    rv;
  }
}

/** Tensor element-wise less than: x :&lt; y. */
case class TensorLTTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a < b) 1.0 else 0.0;
}

/** Tensor element-wise greater than: x :&gt; y. */
case class TensorGTTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a > b) 1.0 else 0.0;
}

/** Tensor element-wise less than or equal to: x :&lt;= y. */
case class TensorLTETensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a <= b) 1.0 else 0.0;
}

/** Tensor element-wise greater than or equal to: x :>= y. */
case class TensorGTETensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a >= b) 1.0 else 0.0;
}

/** Tensor element-wise (strict) equality: x :== y. */
case class TensorEqTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a == b) 1.0 else 0.0;
}

/** Tensor element-wise (strict) non-equality: x :!= y. */
case class TensorNeTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a != b) 1.0 else 0.0;
}

/** Tensor element-wise test for both non-zero: x :&& y. */
case class TensorAndTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 && b != 0) 1.0 else 0.0;
}

/** Tensor element-wise test for either non-zero: x :|| y. */
case class TensorOrTensor[I,Value<:Tensor[I],V2<:Tensor[I],Shape<:TensorShape]
(t1 : TensorOp[Value,Shape], t2 : TensorOp[V2,Shape])
extends TensorTensorFunctionOp[I,Value,V2,Shape](t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 || b != 0) 1.0 else 0.0;
}


class TensorArith[I,Value<:Tensor[I],-V2<:Tensor[I],Shape<:TensorShape] {
  def plus(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorPlusTensor[I,Value,V2,Shape](tensor,tensorB);

  def minus(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorMinusTensor[I,Value,V2,Shape](tensor,tensorB);

  def times(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorMultTensor[I,Value,V2,Shape](tensor,tensorB);

  def div(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorDivTensor[I,Value,V2,Shape](tensor,tensorB);

  def pow(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorPowTensor[I,Value,V2,Shape](tensor,tensorB);

  def lt(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorLTTensor[I,Value,V2,Shape](tensor,tensorB);

  def gt(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorGTTensor[I,Value,V2,Shape](tensor,tensorB);

  def lte(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorLTETensor[I,Value,V2,Shape](tensor,tensorB);

  def gte(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorGTETensor[I,Value,V2,Shape](tensor,tensorB);

  def eq(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorEqTensor[I,Value,V2,Shape](tensor,tensorB);

  def ne(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorNeTensor[I,Value,V2,Shape](tensor,tensorB);

  def and(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorAndTensor[I,Value,V2,Shape](tensor,tensorB);

  def or(tensor : TensorOp[Value,Shape], tensorB : TensorOp[V2,Shape]): TensorOp[Value,Shape] =
    TensorOrTensor[I,Value,V2,Shape](tensor,tensorB);

}

/** Adds together two tensors. */
  case class TensorPlusScalar[Value<:Tensor[_],Shape<:TensorShape]
  (tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorReferenceOp(tensor) {
    override lazy val value : Value = {
      val rv = tensor.working;
      rv += scalar;
      rv;
    }
  }

  //object TensorPlusScalar {
  //  trait Optimizations[Value<:Tensor[_]] extends TensorPlusScalar[Value] {
  //    override def + (s : Double) = (tensor + (scalar + s));
  //    override def - (s : Double) = (tensor + (scalar - s));
  //  }
  //}

  /** Scales a tensor by a scalar: 2 * x. */
  case class TensorMultScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorReferenceOp(tensor) {
    override lazy val value : Value = {
      val rv = tensor.working;
      rv *= scalar;
      rv;
    }
  }

  //object TensorMultScalar {
  //  trait Optimizations[Value<:Tensor[_],Shape<:TensorShape] extends TensorMultScalar[Value] {
  //    override def * (s : Double) = (tensor * (scalar * s));
  //    override def / (s : Double) = (tensor * (scalar / s));
  //  }
  //}

  /** Inverts a tensor and scales: 2 / x. */
  case class ScalarDivTensor[Value<:Tensor[_],Shape<:TensorShape]
  (scalar : Double, val tensor : TensorOp[Value,Shape])
  extends TensorReferenceOp(tensor) {
  //  override def * (s : Double) = ScalarDivTensor(scalar * s, tensor);
  //  override def / (s : Double) = ScalarDivTensor(scalar / s, tensor);
    override lazy val value : Value = {
      val rv = tensor.working;
      rv.default = scalar / rv.default;
      rv.transform((x:Double) => scalar / x);
      rv;
    }
  }

  /** Raises a tensor to a power, element wise: x :^ 2 */
  case class TensorPowScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
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
  case class ScalarPowTensor[Value<:Tensor[_],Shape<:TensorShape]
  (scalar : Double, tensor : TensorOp[Value,Shape])
  extends TensorReferenceOp(tensor) {
    override lazy val value : Value = {
      val rv = tensor.working;
      rv.default = Math.pow(scalar, rv.default);
      rv.transform((x:Double) => Math.pow(scalar, x));
      rv;
    }
  }

  /** Tensor less than a scalar: x &lt; 2. */
  case class TensorLTScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x < scalar) 1.0 else 0.0;
  }

  /** Tensor greater than a scalar: x > 2. */
  case class TensorGTScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x > scalar) 1.0 else 0.0;
  }

  /** Tensor less than or equal to a scalar: x <= 2. */
  case class TensorLTEScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x <= scalar) 1.0 else 0.0;
  }

  /** Tensor greater than or equal to a scalar: x >= 2. */
  case class TensorGTEScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x >= scalar) 1.0 else 0.0;
  }

  /** Tensor equal to a scalar in each element: x :== 2. */
  case class TensorEqScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x == scalar) 1.0 else 0.0;
  }

  /** Tensor not equal to a scalar in each element: x :!= 2. */
  case class TensorNeScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x != scalar) 1.0 else 0.0;
  }

  /** Either tensor or scalar is non-zero in each position: x :|| 0. */
  case class TensorOrScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x != 0.0 || scalar != 0) 1.0 else 0.0;
  }

  /** The negation of a tensor. */
  case class TensorNegation[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape])
  extends TensorReferenceOp[Value,Shape](tensor) {
    override def value = {
      val rv = tensor.working;
      rv *= -1;
      rv;
    }
  }

  /** Both tensor and scalar are non-zero in each position: x :&& 1. */
  case class TensorAndScalar[Value<:Tensor[_],Shape<:TensorShape]
  (val tensor : TensorOp[Value,Shape], scalar : Double)
  extends TensorFunctionOp(tensor) {
    override def function(x : Double) = if (x != 0.0 && scalar != 0) 1.0 else 0.0;
  }

