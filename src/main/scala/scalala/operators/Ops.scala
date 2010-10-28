/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */

package scalala;
package operators;

import generic._;
import generic.collection.CanTranspose;

import scalala.tensor.Scalar;
import scalala.tensor.domain.DomainException;
import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

/**
 * Mix-in trait for supporting numeric operators on instances of type This
 * (accessible with the repr method).
 *
 * @author dramage
 */
trait NumericOps[+This] {
  def repr : This;

  def unary_-[TT>:This,That](implicit op : CanNeg[TT,That]) : That = op(repr);

  def :+[TT>:This,B,That](b : B)(implicit op : CanAdd[TT,B,That]) = op(repr,b);

  def :-[TT>:This,B,That](b : B)(implicit op : CanSub[TT,B,That]) = op(repr,b);

  def :*[TT>:This,B,That](b : B)(implicit op : CanMul[TT,B,That]) = op(repr,b);

  def :/[TT>:This,B,That](b : B)(implicit op : CanDiv[TT,B,That]) = op(repr,b);

  def :%[TT>:This,B,That](b : B)(implicit op : CanMod[TT,B,That]) = op(repr,b);

  def :^[TT>:This,B,That](b : B)(implicit op : CanPow[TT,B,That]) = op(repr,b);

  def :<[TT>:This,B,That](b : B)(implicit op : CanLT[TT,B,That]) = op(repr,b);

  def :<=[TT>:This,B,That](b : B)(implicit op : CanLTE[TT,B,That]) = op(repr,b);

  def :>[TT>:This,B,That](b : B)(implicit op : CanGT[TT,B,That]) = op(repr,b);

  def :>=[TT>:This,B,That](b : B)(implicit op : CanGTE[TT,B,That]) = op(repr,b);

  def :==[TT>:This,B,That](b : B)(implicit op : CanEq[TT,B,That]) = op(repr,b);

  def :!=[TT>:This,B,That](b : B)(implicit op : CanNe[TT,B,That]) = op(repr,b);

  //
  // Scalar operator aliases
  //

  /** Alias for :+=(b) */
  final def +[TT>:This,B,That](b : B)(implicit op : CanAdd[TT,B,That], sb : Scalar[B]) =
    this.:+[TT,B,That](b);

  /** Alias for :-=(b) */
  final def -[TT>:This,B,That](b : B)(implicit op : CanSub[TT,B,That], sb : Scalar[B]) =
    this.:-[TT,B,That](b);
}

/**
 * Operators for mutable numeric collections.
 *
 * @author dramage
 */
trait MutableNumericOps[+This] extends NumericOps[This] {
  def repr : This;

  def :=[TT>:This,B](b : B)(implicit op : CanAssignInto[TT,B]) = op(repr,b);

  def :+=[TT>:This,B](b : B)(implicit op : CanAddInto[TT,B]) = op(repr,b);

  def :-=[TT>:This,B](b : B)(implicit op : CanSubInto[TT,B]) = op(repr,b);

  def :*=[TT>:This,B](b : B)(implicit op : CanMulInto[TT,B]) = op(repr,b);

  def :/=[TT>:This,B](b : B)(implicit op : CanDivInto[TT,B]) = op(repr,b);

  def :%=[TT>:This,B](b : B)(implicit op : CanModInto[TT,B]) = op(repr,b);

  def :^=[TT>:This,B](b : B)(implicit op : CanPowInto[TT,B]) = op(repr,b);

  /** Alias for :+=(b). */
  final def +=[TT>:This,B](b : B)(implicit op : CanAddInto[TT,B], sb : Scalar[B]) =
    this.:+=[TT,B](b);

  /** Alias for :-=(b). */
  final def -=[TT>:This,B](b : B)(implicit op : CanSubInto[TT,B], sb : Scalar[B]) =
    this.:-=[TT,B](b);

  /** Alias for :*=(b). */
  final def *=[TT>:This,B](b : B)(implicit op : CanMulInto[TT,B], sb : Scalar[B]) =
    this.:*=[TT,B](b);

  /** Alias for :/=(b). */
  final def /=[TT>:This,B](b : B)(implicit op : CanDivInto[TT,B], sb : Scalar[B]) =
    this.:/=[TT,B](b);

  /** Alias for :%=(b). */
  final def %=[TT>:This,B](b : B)(implicit op : CanModInto[TT,B], sb : Scalar[B]) =
    this.:%=[TT,B](b);
}

//
// Shaped operations
// 


/**
 * Secialized NumericOps with shaped operations taking A is a column.
 * Note that columns are the default shape, so this trait simply extends
 * NumericOps[A].
 *
 * @author dramage
 */
trait ColumnTensorOps[+This] extends NumericOps[This] {
  def *[TT>:This,B,RV](b : RowTensorOps[B])(implicit op : CanMulColumnBy[TT,B,RV]) : RV =
    op(repr,b.column);

  def t : RowTensorOps[This] = RowTensorOps(repr);
}

/**
 * A column tensor whose underyling collection is mutable.  This trait
 * should be used instead of mixing in "ColumnTensorOps with
 * MutableNumeriCollectionOps" directly, because .t needs to return an instance
 * of MutableRowTensorOps insetad of RowTensorOps.
 *
 * @author dramage
 */
trait MutableColumnTensorOps[+This] extends ColumnTensorOps[This] with MutableNumericOps[This] {
  override def t : MutableRowTensorOps[This] = MutableRowTensorOps(repr);
}


/*
 * Secialized NumericOps with shaped operations taking A is a row.
 * Note that there is an inherent asymmetry between ColumnTensorOps and
 * RowTensorOps: because tensors are assumed to be columns until reshaped
 * (e.g. by calling .t), that class extends NumericOps[A].  This
 * class, by contrast, must preserve the fact that the base numeric operations
 * like plus must honor the row shape, and that the return result should also
 * be a row.  Hence this class extends NumericOps[RowTensorOps[A]]
 * and provides implicit magic in the companion object to wrap the
 * corresponding construction delegates.
 *
 * @author dramage
 */
trait RowTensorOps[+This] extends NumericOps[RowTensorOps[This]] {
  override def repr : RowTensorOps[This] = this;

  def column : This;

  def *[TT>:This,B,RV](b : B)(implicit op : CanMulRowBy[TT,B,RV]) : RV =
    op(this.column,b);

  /** The transpose returns the underlying value, which assumed to be a column. */
  def t : This = column;
}

object RowTensorOps {
  def apply[This](v : This) : RowTensorOps[This] =
    new RowTensorOps[This] { override def column = v; }

  class RowBinaryOp[A,-B,+That](implicit op : BinaryOp[A,B,That])
  extends BinaryOp[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]] {
    override def apply(a : RowTensorOps[A], b : RowTensorOps[B]) =
      RowTensorOps(op(a.column,b.column));
  }

  implicit def CanAddRows[A,B,That](implicit op : CanAdd[A,B,That])
  = new RowBinaryOp[A,B,That] with CanAdd[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanSubRows[A,B,That](implicit op : CanSub[A,B,That])
  = new RowBinaryOp[A,B,That] with CanSub[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanMulRows[A,B,That](implicit op : CanMul[A,B,That])
  = new RowBinaryOp[A,B,That] with CanMul[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanDivRows[A,B,That](implicit op : CanDiv[A,B,That])
  = new RowBinaryOp[A,B,That] with CanDiv[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanModRows[A,B,That](implicit op : CanMod[A,B,That])
  = new RowBinaryOp[A,B,That] with CanMod[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanPowRows[A,B,That](implicit op : CanPow[A,B,That])
  = new RowBinaryOp[A,B,That] with CanPow[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanLTRows[A,B,That](implicit op : CanLT[A,B,That])
  = new RowBinaryOp[A,B,That] with CanLT[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanLTERows[A,B,That](implicit op : CanLTE[A,B,That])
  = new RowBinaryOp[A,B,That] with CanLTE[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanGTRows[A,B,That](implicit op : CanGT[A,B,That])
  = new RowBinaryOp[A,B,That] with CanGT[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanGTERows[A,B,That](implicit op : CanGTE[A,B,That])
  = new RowBinaryOp[A,B,That] with CanGTE[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanEqRows[A,B,That](implicit op : CanEq[A,B,That])
  = new RowBinaryOp[A,B,That] with CanEq[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];

  implicit def CanNeRows[A,B,That](implicit op : CanNe[A,B,That])
  = new RowBinaryOp[A,B,That] with CanNe[RowTensorOps[A],RowTensorOps[B],RowTensorOps[That]];
}

/**
 * Specialized RowTensorOps support for RowTensors that have mutable
 * underlying collections.
 *
 * @author dramage
 */
trait MutableRowTensorOps[+This]
extends RowTensorOps[This] with MutableNumericOps[RowTensorOps[This]];

object MutableRowTensorOps {
  def apply[This](v : This) : MutableRowTensorOps[This] =
    new MutableRowTensorOps[This] { override def column = v; }

  class MutableRowBinaryOp[A,-B,+That](implicit op : BinaryOp[A,B,That])
  extends BinaryOp[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]] {
    override def apply(a : MutableRowTensorOps[A], b : RowTensorOps[B]) =
      MutableRowTensorOps(op(a.column,b.column));
  }

  class MutableRowBinaryUpdateOp[A,-B](implicit op : BinaryUpdateOp[A,B])
  extends BinaryUpdateOp[MutableRowTensorOps[A],RowTensorOps[B]] {
    override def apply(a : MutableRowTensorOps[A], b : RowTensorOps[B]) =
      op(a.column, b.column);
  }

  implicit def CanAddRows[A,B,That](implicit op : CanAdd[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanAdd[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanSubRows[A,B,That](implicit op : CanSub[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanSub[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanMulRows[A,B,That](implicit op : CanMul[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanMul[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanDivRows[A,B,That](implicit op : CanDiv[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanDiv[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanModRows[A,B,That](implicit op : CanMod[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanMod[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanPowRows[A,B,That](implicit op : CanPow[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanPow[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanLTRows[A,B,That](implicit op : CanLT[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanLT[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanLTERows[A,B,That](implicit op : CanLTE[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanLTE[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanGTRows[A,B,That](implicit op : CanGT[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanGT[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanGTERows[A,B,That](implicit op : CanGTE[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanGTE[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanEqRows[A,B,That](implicit op : CanEq[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanEq[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];

  implicit def CanNeRows[A,B,That](implicit op : CanNe[A,B,That])
  = new MutableRowBinaryOp[A,B,That] with CanNe[MutableRowTensorOps[A],RowTensorOps[B],MutableRowTensorOps[That]];
}

/**
 * Provides matrix-like operations for two dimensional collections.
 *
 * @author dramage
 */
trait MatrixOps[+This] extends NumericOps[This] {
  def *[TT>:This,B,That](b : B)(implicit op : CanMulMatrixBy[TT,B,That]) =
    op.apply(repr,b);

  def t[TT>:This,That](implicit op : CanTranspose[TT,That]) =
    op.apply(repr);
}

/**
 * Numeric operator support for numeric arrays.
 *
 * @author dramage
 */
class RichArrayVector[V:ClassManifest](override val repr : Array[V])
extends MutableColumnTensorOps[Array[V]] {
  /** Final alias for :+ as a workaround for arrays. */
  final def :+:[B,That](b : B)(implicit op : CanAdd[Array[V],B,That]) = this.:+(b);
}

/**
 * Numeric operator support for Array[Array] matrix.
 * 
 * @author dramage
 */
class RichArrayMatrix[V:ClassManifest](override val repr : Array[Array[V]])
extends MatrixOps[Array[Array[V]]];

/**
 * Numeric operator support for SparseArray vector.
 *
 * @author dramage
 */
class RichSparseArrayVector[V:ClassManifest:DefaultArrayValue](override val repr : SparseArray[V])
extends MutableColumnTensorOps[SparseArray[V]];

/**
 * Numeric operator support for solo scalars.  Note: we do not support
 * raw "+" to avoid ambiguity with the any2String implicit that comes built
 * in with scala and is used for string concatenation.  Use :+ instead.
 * Similarly, use ":-" instead of "-", which we exclude for consistency.
 *
 * @author dramage
 */
class RichScalar[@specialized(Int,Long,Float,Double) A:Scalar]
(override val repr : A)
extends NumericOps[A];

class RichTuple2[@specialized A, @specialized B]
(override val repr : (A,B))
extends NumericOps[(A,B)];

class RichTuple3[@specialized A, @specialized B, @specialized C]
(override val repr : (A,B,C))
extends NumericOps[(A,B,C)];

class RichTuple4[@specialized A, @specialized B, @specialized C, @specialized D]
(override val repr : (A,B,C,D))
extends NumericOps[(A,B,C,D)];

/**
 * Numeric operator support for scala maps.
 * 
 * @author dramage
 */
class RichMap[K,V](override val repr : Map[K,V])
extends NumericOps[Map[K,V]];

object Implicits {
  implicit def richScalar[@specialized(Int,Long,Float,Double) V:Scalar](value : V) =
    new RichScalar(value);

  implicit def richTuple2[@specialized A, @specialized B](value : (A,B)) =
    new RichTuple2(value);

  implicit def richTuple3[@specialized A, @specialized B, @specialized C](value : (A,B,C)) =
    new RichTuple3(value);

  implicit def richTuple4[@specialized A, @specialized B, @specialized C, @specialized D](value : (A,B,C,D)) =
    new RichTuple4(value);

  implicit def richArrayVector[V:ClassManifest](value : Array[V]) =
    new RichArrayVector(value);

  implicit def richSparseArrayVector[V:ClassManifest:DefaultArrayValue](value : SparseArray[V]) =
    new RichSparseArrayVector(value);

  implicit def richArrayMatrix[V:ClassManifest](value : Array[Array[V]]) =
    new RichArrayMatrix(value);

  implicit def richMap[K,V](value : Map[K,V]) =
    new RichMap[K,V](value);
}
