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
  protected val innerTensorOpBuilder =
    new TensorOpBuilder[Any,Tensor[Any],Any]();
  
  implicit def iTensorOpBuilder[I] =
    innerTensorOpBuilder.asInstanceOf[TensorOpBuilder[I,Tensor[I],Any]];

  implicit def iTensorOpToRichTensorOp[I]
  (op : TensorOp[I,Tensor[I],Tensor[I],Any])
  (implicit builder : TensorOpBuilder[I,Tensor[I],Any]) =
    new RichTensorOp[I,Tensor[I],Tensor[I],Any](op)(builder);

  implicit def iTensorToRichTensorOp[I](x : Tensor[I])
  (implicit builder : TensorOpBuilder[I,Tensor[I],Any]) =
    new RichTensorOp[I,Tensor[I],Tensor[I],Any](builder.mkTensorIdentity(x))(builder);
  
  implicit def iScalarToRichScalarTensorOp(s : Double) =
    new RichScalarTensorOp(s);
  
  implicit def iTensorOpToTensor[I,Base<:Tensor[I],V<:Base]
  (x : TensorOp[I,Base,V,_]) : Tensor[I] =
    x.value.asInstanceOf[V];
}

/** Singleton instance of TensorOps trait. */
object TensorOps extends TensorOps;

//
// TODO: change Shape stuff to use these shapes for better iTensorOpToTensor
//
//object TensorOp {
//  abstract sealed class Shape;
//  abstract sealed class AssignableShape;
//  abstract sealed class IntermediateShape;
//  abstract final class AnyShape;
//}

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
trait TensorOp[I,Bound<:Tensor[I],Value<:Bound,Shape] {
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
 * A builder for making new operators.  This builder is
 * used instead of constructing case classes directly
 * because it allows for straightforward additions of
 * extra optimizations, etc., by building in cleverer
 * builders.
 * 
 * @author dramage
 */
class TensorOpBuilder[I,Bound<:Tensor[I],Shape] {
  
  def mkTensorIdentity[Value<:Bound](tensor : Value) =
    TensorIdentity[I,Bound,Value,Shape](tensor);
  
  def mkTensorNegation[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape]) =
    TensorNegation[I,Bound,Value,Shape](tensor);
  
  def mkTensorPlusScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorPlusScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorMultScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorMultScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkScalarDivTensor[Value<:Bound](scalar : Double, tensor : TensorOp[I,Bound,Value,Shape]) =
    ScalarDivTensor[I,Bound,Value,Shape](scalar, tensor);
  
  def mkTensorPowScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorPowScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkScalarPowTensor[Value<:Bound](scalar : Double, tensor : TensorOp[I,Bound,Value,Shape]) =
    ScalarPowTensor[I,Bound,Value,Shape](scalar, tensor);
  
  def mkTensorLTScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorLTScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorGTScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorGTScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorLTEScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorLTEScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorGTEScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorGTEScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorAndScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorAndScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorOrScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorOrScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorEqScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorEqScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorNeScalar[Value<:Bound](tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) =
    TensorNeScalar[I,Bound,Value,Shape](tensor, scalar);
  
  def mkTensorPlusTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorPlusTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorMinusTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorMinusTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);
  
  def mkTensorMultTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorMultTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorDivTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorDivTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorPowTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorPowTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorLTTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorLTTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorGTTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorGTTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorLTETensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorLTETensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorGTETensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorGTETensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorEqTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorEqTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorNeTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorNeTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorAndTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorAndTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);

  def mkTensorOrTensor[V1<:Bound,V2<:Bound](tensorA : TensorOp[I,Bound,V1,Shape], tensorB : TensorOp[I,Bound,V2,Shape]) =
    TensorOrTensor[I,Bound,V1,V2,Shape](tensorA, tensorB);
}

/**
 * Operations applicable to any TensorOp via tensor-scalar and
 * tensor-tensor arithmetic.  Types are left-associative -- the
 * value of say a DenseVector + Vector will be a DenseVector,
 * but a Vector + DenseVector will be a Vector.
 * 
 * @author dramage
 */
class RichTensorOp[I,Bound<:Tensor[I],Value<:Bound,Shape]
(base : TensorOp[I,Bound,Value,Shape])
(implicit ops : TensorOpBuilder[I,Bound,Shape]) {
  
  /** Unary minus returns a tensor negation. */
  def unary_- = ops.mkTensorNegation(base);
  
  //
  // scalar operators
  //
  
  def  + (s : Double) = ops.mkTensorPlusScalar(base, s);
  def  - (s : Double) = ops.mkTensorPlusScalar(base, -s);
  def  * (s : Double) = ops.mkTensorMultScalar(base, s);
  def  / (s : Double) = ops.mkTensorMultScalar(base, 1.0 / s);
  def :^ (s : Double) = ops.mkTensorPowScalar(base, s);
  def  < (s : Double) = ops.mkTensorLTScalar(base, s);
  def  > (s : Double) = ops.mkTensorGTScalar(base, s);
  def <= (s : Double) = ops.mkTensorLTEScalar(base, s);
  def >= (s : Double) = ops.mkTensorGTEScalar(base, s);
  def && (s : Double) = ops.mkTensorAndScalar(base, s);
  def || (s : Double) = ops.mkTensorOrScalar(base, s);
  
  /** Colon prefix is required to avoid conflation with .equals */
  def :== (s : Double) = ops.mkTensorEqScalar(base, s);
  
  /** Colon prefix is required to avoid conflation with .equals */
  def :!= (s : Double) = ops.mkTensorNeScalar(base, s);
  
  //
  // tensor operators
  //
    
  def :+ [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorPlusTensor(base, op);
  def :+ [V<:Bound] (v  : V) =
    ops.mkTensorPlusTensor(base, ops.mkTensorIdentity(v));
  
  def :- [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorMinusTensor(base, op);
  def :- [V<:Bound] (v : V) =
    ops.mkTensorMinusTensor(base, ops.mkTensorIdentity(v));
  
  def :*  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorMultTensor(base, op);
  def :* [V<:Bound] (v : V) =
    ops.mkTensorMultTensor(base, ops.mkTensorIdentity(v));
  
  def :/  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorDivTensor(base, op);
  def :/ [V<:Bound] (v : V) =
    ops.mkTensorDivTensor(base, ops.mkTensorIdentity(v));
  
  def :^  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorPowTensor(base, op);
  def :^ [V<:Bound] (v : V) =
    ops.mkTensorPowTensor(base, ops.mkTensorIdentity(v));
  
  def :<  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorLTTensor(base, op);
  def :< [V<:Bound] (v : V) =
    ops.mkTensorLTTensor(base, ops.mkTensorIdentity(v));
  
  def :>  [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorGTTensor(base, op);
  def :> [V<:Bound] (v : V) =
    ops.mkTensorGTTensor(base, ops.mkTensorIdentity(v));
  
  def :<= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorLTETensor(base, op);
  def :<= [V<:Bound] (v : V) =
    ops.mkTensorLTETensor(base, ops.mkTensorIdentity(v));
  
  def :>= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorGTETensor(base, op);
  def :>= [V<:Bound] (v : V) =
    ops.mkTensorGTETensor(base, ops.mkTensorIdentity(v));
  
  def :== [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorEqTensor(base, op);
  def :== [V<:Bound] (v : V) =
    ops.mkTensorEqTensor(base, ops.mkTensorIdentity(v));
  
  def :!= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorNeTensor(base, op);
  def :!= [V<:Bound] (v : V) =
    ops.mkTensorNeTensor(base, ops.mkTensorIdentity(v));
  
  def :&& [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorAndTensor(base, op);
  def :&& [V<:Bound] (v : V) =
    ops.mkTensorAndTensor(base, ops.mkTensorIdentity(v));
  
  def :|| [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) =
    ops.mkTensorOrTensor(base, op);
  def :|| [V<:Bound] (v : V) =
    ops.mkTensorOrTensor(base, ops.mkTensorIdentity(v));
  
  /** Fixed alias for :+ */
  final def + [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :+ op;
  /** Fixed alias for :+ */
  final def + [V<:Bound] (v : V) = this :+ v;
  
  /** Fixed alias for :- */
  final def - [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :- op;
  /** Fixed alias for :- */
  final def - [V<:Bound] (v : V) = this :- v;
  
  /** Fixed alias for :< */
  final def < [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :< op;
  /** Fixed alias for :< */
  final def < [V<:Bound] (v : V) = this :< v;
  
  /** Fixed alias for :> */
  final def > [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :> op;
  /** Fixed alias for :> */
  final def > [V<:Bound] (v : V) = this :> v;
  
  /** Fixed alias for :<= */
  final def <= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :<= op;
  /** Fixed alias for :<= */
  final def <= [V<:Bound] (v : V) = this :<= v;
  
  /** Fixed alias for :>= */
  final def >= [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :>= op;
  /** Fixed alias for :>= */
  final def >= [V<:Bound] (v : V) = this :>= v;
  
  /** Fixed alias for :&& */
  final def && [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :&& op;
  /** Fixed alias for :&& */
  final def && [V<:Bound] (v : V) = this :&& v;
  
  /** Fixed alias for :|| */
  final def || [V<:Bound] (op : TensorOp[I,Bound,V,Shape]) = this :|| op;
  /** Fixed alias for :|| */
  final def || [V<:Bound] (v : V) = this :|| v;
}

/**
 * A rich scalar extension with support for tensor operators.
 */
class RichScalarTensorOp(s : Double) {
  
  def  + [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorPlusScalar(base, s);
  
  final def  + [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorPlusScalar(base, s);
  }
  
  def  - [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorPlusScalar(ops.mkTensorNegation(base), s);
  
  final def  - [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorPlusScalar(ops.mkTensorNegation(base), s);
  }
  
  def  * [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorMultScalar(base, s);
  
  final def  * [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorMultScalar(base, s);
  }
  
  def  / [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkScalarDivTensor(s, base);
  
  final def  / [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkScalarDivTensor(s, base);
  }
  
  def :^ [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkScalarPowTensor(s, base);
  
  final def :^ [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkScalarPowTensor(s, base);
  }
  
  def  < [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorGTScalar(base, s);
  
  final def  < [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorGTScalar(base, s);
  }
  
  def  > [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorLTScalar(base, s);
  
  final def  > [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorLTScalar(base, s);
  }
  
  def <= [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorGTEScalar(base, s);
  
  final def  <= [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorGTEScalar(base, s);
  }
  
  def >= [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorLTEScalar(base, s);
  
  final def  >= [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorLTEScalar(base, s);
  }
  
  def && [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorAndScalar(base, s);
  
  final def  && [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorAndScalar(base, s);
  }
  
  def || [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorOrScalar(base, s);
  
  final def  || [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorOrScalar(base, s);
  }
  
  /** Colon prefix is required to avoid conflation with .equals */
  def :== [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorEqScalar(base, s);
  
  final def :== [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorEqScalar(base, s);
  }
  
  /** Colon prefix is required to avoid conflation with .equals */
  def :!= [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (base : TensorOp[I,Bound,Value,Shape])
  (implicit ops : TensorOpBuilder[I,Bound,Shape]) =
    ops.mkTensorNeScalar(base, s);
  
  final def :!= [I,Bound<:Tensor[I],Value<:Bound,Shape]
  (t : Value)(implicit ops : TensorOpBuilder[I,Bound,Shape]) = {
    val base = ops.mkTensorIdentity(t);
    ops.mkTensorNeScalar(base, s);
  }
}

/** A reference to an underlying tensor. */
case class TensorIdentity[I,Bound<:Tensor[I],Value<:Bound,Shape]
(tensor : Value) extends TensorOp[I,Bound,Value,Shape] {  
  override def domain = tensor.domain;
  override def value = tensor;
  override def working = tensor.copy.asInstanceOf[Value];
  override def create[J](d : MergeableSet[J]) = tensor.create(d);
}

/** A reference to an underlying TensorOp. */
abstract case class TensorReferenceOp[I,Bound<:Tensor[I],Value<:Bound,Shape]
(tensor : TensorOp[I,Bound,Value,Shape])
extends TensorOp[I,Bound,Value,Shape] {
  override def domain = tensor.domain;
  override def create[J](d : MergeableSet[J]) = tensor.create(d);
}
  
/** An operation applied to a tensor's values. */
abstract case class TensorFunctionOp[I,Bound<:Tensor[I],Value<:Bound,Shape]
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
abstract class TensorTensorOp[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,V1,Shape], val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorReferenceOp[I,Bound,V1,Shape](tensor) {
  if (tensor.domain != tensorB.domain) throw new DomainException("Tensors have incompatible domain");
}

/** An abstract operator between two tensors and a function of their values. */
abstract class TensorTensorFunctionOp[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
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
case class TensorNegation[I,Bound<:Tensor[I],Value<:Bound,Shape]
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
case class TensorPlusScalar[I,Bound<:Tensor[I],Value<:Bound,Shape](override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double) extends TensorReferenceOp(tensor) {
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
case class TensorMultScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
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
case class ScalarDivTensor[I,Bound<:Tensor[I],Value<:Bound,Shape]
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
case class TensorPowScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
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
case class ScalarPowTensor[I,Bound<:Tensor[I],Value<:Bound,Shape]
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
case class TensorLTScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x < scalar) 1.0 else 0.0;
}

/** Tensor greater than a scalar: x > 2. */
case class TensorGTScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x > scalar) 1.0 else 0.0;
}

/** Tensor less than or equal to a scalar: x <= 2. */
case class TensorLTEScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x <= scalar) 1.0 else 0.0;
}

/** Tensor greater than or equal to a scalar: x >= 2. */
case class TensorGTEScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x >= scalar) 1.0 else 0.0;
}

/** Tensor equal to a scalar in each element: x :== 2. */
case class TensorEqScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x == scalar) 1.0 else 0.0;
}

/** Tensor not equal to a scalar in each element: x :!= 2. */
case class TensorNeScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != scalar) 1.0 else 0.0;
}

/** Either tensor or scalar is non-zero in each position: x :|| 0. */
case class TensorOrScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != 0.0 || scalar != 0) 1.0 else 0.0;
}

/** Both tensor and scalar are non-zero in each position: x :&& 1. */
case class TensorAndScalar[I,Bound<:Tensor[I],Value<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,Value,Shape], scalar : Double)
extends TensorFunctionOp(tensor) {
  override def function(x : Double) = if (x != 0.0 && scalar != 0) 1.0 else 0.0;
}

/** Tensor addition: x + y. */
case class TensorPlusTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :+= tensorB.value;
    rv;
  }
}

/** Tensor subtraction: x - y. */
case class TensorMinusTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :-= tensorB.value;
    rv;
  }
}

/** Tensor element-wise multiplication: x :* y. */
case class TensorMultTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :*= tensorB.value;
    rv;
  }
}

/** Tensor element-wise division: x :/ y. */
case class TensorDivTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :/= tensorB.value;
    rv;
  }
}

/** Tensor element-wise exponentiatoin: x :^ y. */
case class TensorPowTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(override val tensor : TensorOp[I,Bound,V1,Shape], override val tensorB : TensorOp[I,Bound,V2,Shape])
extends TensorTensorOp(tensor,tensorB) {
  override lazy val value : V1 = {
    val rv = tensor.working;
    rv :^= tensorB.value;
    rv;
  }
}

/** Tensor element-wise less than: x :< y. */
case class TensorLTTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a < b) 1.0 else 0.0;
}

/** Tensor element-wise greater than: x :> y. */
case class TensorGTTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a > b) 1.0 else 0.0;
}

/** Tensor element-wise less than or equal to: x :<= y. */
case class TensorLTETensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a <= b) 1.0 else 0.0;
}

/** Tensor element-wise greater than or equal to: x :>= y. */
case class TensorGTETensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a >= b) 1.0 else 0.0;
}

/** Tensor element-wise (strict) equality: x :== y. */
case class TensorEqTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a == b) 1.0 else 0.0;
}

/** Tensor element-wise (strict) non-equality: x :!= y. */
case class TensorNeTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != b) 1.0 else 0.0;
}

/** Tensor element-wise test for both non-zero: x :&& y. */
case class TensorAndTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 && b != 0) 1.0 else 0.0;
}

/** Tensor element-wise test for either non-zero: x :|| y. */
case class TensorOrTensor[I,Bound<:Tensor[I],V1<:Bound,V2<:Bound,Shape]
(t1 : TensorOp[I,Bound,V1,Shape], t2 : TensorOp[I,Bound,V2,Shape])
extends TensorTensorFunctionOp(t1,t2) {
  override def function(a : Double, b : Double) = if (a != 0 || b != 0) 1.0 else 0.0;
}
