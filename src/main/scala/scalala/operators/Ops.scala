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

import scalala.scalar.Scalar;
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

  /** Alias for :*=(b) */
  final def *[TT>:This,B,That](b : B)(implicit op : CanMul[TT,B,That], sb : Scalar[B]) =
    this.:*[TT,B,That](b);

  /** Alias for :/=(b) */
  final def /[TT>:This,B,That](b : B)(implicit op : CanDiv[TT,B,That], sb : Scalar[B]) =
    this.:/[TT,B,That](b);

  /** Alias for :%=(b) */
  final def %[TT>:This,B,That](b : B)(implicit op : CanMod[TT,B,That], sb : Scalar[B]) =
    this.:%[TT,B,That](b);
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
 * Secialized shaped numeric operations for columns.
 *
 * @author dramage
 */
trait ColOps[+This] extends NumericOps[This] {
  def *[TT>:This,B,That](b : B)(implicit op : CanMulColumnBy[TT,B,That]) : That =
    op(repr, b);

  def t[TT>:This,That](implicit op : CanTranspose[TT,That]) =
    op.apply(repr);
}

/**
 * Secialized shaped numeric operations for rows.
 *
 * @author dramage
 */
trait RowOps[+This] extends NumericOps[This] {
  def *[TT>:This,B,That](b : B)(implicit op : CanMulRowBy[TT,B,That]) : That =
    op(repr, b);

  def t[TT>:This,That](implicit op : CanTranspose[TT,That]) =
    op.apply(repr);
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
 * Specialized column and row tensor ops for wrapped data structures that
 * cannot directly inherit and distinguish between rows and columns in the
 * type system.  This class assumes unwrapped objects are columsn and
 * wrapped ones are rows.
 *
 * @author dramage
 */
trait WrappedColOps[+This] extends NumericOps[This] {
  def *[TT>:This,B,RV](b : WrappedRowOps[B])(implicit op : CanMulColumnBy[TT,B,RV]) : RV =
    op(repr,b.column);

  def t : WrappedRowOps[This] = WrappedRowOps(repr);
}

/**
 * A wrapped column tensor whose underyling collection is mutable.  This trait
 * should be used instead of mixing in "WrappedColOps with
 * MutableNumeriCollectionOps" directly, because .t needs to return an instance
 * of MutableRowOps insetad of RowOps.
 *
 * @author dramage
 */
trait MutableWrappedColOps[+This] extends WrappedColOps[This] with MutableNumericOps[This] {
  override def t : MutableWrappedRowOps[This] = MutableWrappedRowOps(repr);
}


/**
 * Secialized NumericOps with shaped operations taking A is a row.
 * Note that there is an inherent asymmetry between WrappedColumnTensorOps and
 * WrappedRowOps: because tensors are assumed to be columns until reshaped
 * (e.g. by calling .t), that class extends NumericOps[A].  This
 * class, by contrast, must preserve the fact that the base numeric operations
 * like plus must honor the row shape, and that the return result should also
 * be a row.  Hence this class extends NumericOps[WrappedRowOps[A]]
 * and provides implicit magic in the companion object to wrap the
 * corresponding construction delegates.
 *
 * @author dramage
 */
trait WrappedRowOps[+This] extends NumericOps[WrappedRowOps[This]] {
  override def repr : WrappedRowOps[This] = this;

  def column : This;

  def *[TT>:This,B,RV](b : B)(implicit op : CanMulRowBy[TT,B,RV]) : RV =
    op(this.column,b);

  /** The transpose returns the underlying value, which assumed to be a column. */
  def t : This = column;
}

object WrappedRowOps {
  def apply[This](v : This) : WrappedRowOps[This] =
    new WrappedRowOps[This] { override def column = v; }

  class WrappedRowBinaryOp[A,-B,+That](implicit op : BinaryOp[A,B,That])
  extends BinaryOp[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]] {
    override def apply(a : WrappedRowOps[A], b : WrappedRowOps[B]) =
      WrappedRowOps(op(a.column,b.column));
  }

  implicit def CanAddRows[A,B,That](implicit op : CanAdd[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanAdd[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanSubRows[A,B,That](implicit op : CanSub[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanSub[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanMulRows[A,B,That](implicit op : CanMul[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanMul[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanDivRows[A,B,That](implicit op : CanDiv[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanDiv[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanModRows[A,B,That](implicit op : CanMod[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanMod[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanPowRows[A,B,That](implicit op : CanPow[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanPow[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanLTRows[A,B,That](implicit op : CanLT[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanLT[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanLTERows[A,B,That](implicit op : CanLTE[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanLTE[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanGTRows[A,B,That](implicit op : CanGT[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanGT[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanGTERows[A,B,That](implicit op : CanGTE[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanGTE[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanEqRows[A,B,That](implicit op : CanEq[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanEq[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];

  implicit def CanNeRows[A,B,That](implicit op : CanNe[A,B,That])
  = new WrappedRowBinaryOp[A,B,That] with CanNe[WrappedRowOps[A],WrappedRowOps[B],WrappedRowOps[That]];
}

/**
 * Specialized WrappedRowOps support for WrappedRows that have mutable
 * underlying collections.
 *
 * @author dramage
 */
trait MutableWrappedRowOps[+This]
extends WrappedRowOps[This] with MutableNumericOps[WrappedRowOps[This]];

object MutableWrappedRowOps {
  def apply[This](v : This) : MutableWrappedRowOps[This] =
    new MutableWrappedRowOps[This] { override def column = v; }

  class MutableWrappedRowBinaryOp[A,-B,+That](implicit op : BinaryOp[A,B,That])
  extends BinaryOp[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]] {
    override def apply(a : MutableWrappedRowOps[A], b : WrappedRowOps[B]) =
      MutableWrappedRowOps(op(a.column,b.column));
  }

  class MutableWrappedRowBinaryUpdateOp[A,-B](implicit op : BinaryUpdateOp[A,B])
  extends BinaryUpdateOp[MutableWrappedRowOps[A],WrappedRowOps[B]] {
    override def apply(a : MutableWrappedRowOps[A], b : WrappedRowOps[B]) =
      op(a.column, b.column);
  }

  implicit def CanAddRows[A,B,That](implicit op : CanAdd[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanAdd[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanSubRows[A,B,That](implicit op : CanSub[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanSub[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanMulRows[A,B,That](implicit op : CanMul[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanMul[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanDivRows[A,B,That](implicit op : CanDiv[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanDiv[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanModRows[A,B,That](implicit op : CanMod[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanMod[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanPowRows[A,B,That](implicit op : CanPow[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanPow[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanLTRows[A,B,That](implicit op : CanLT[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanLT[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanLTERows[A,B,That](implicit op : CanLTE[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanLTE[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanGTRows[A,B,That](implicit op : CanGT[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanGT[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanGTERows[A,B,That](implicit op : CanGTE[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanGTE[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanEqRows[A,B,That](implicit op : CanEq[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanEq[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];

  implicit def CanNeRows[A,B,That](implicit op : CanNe[A,B,That])
  = new MutableWrappedRowBinaryOp[A,B,That] with CanNe[MutableWrappedRowOps[A],WrappedRowOps[B],MutableWrappedRowOps[That]];
}

/**
 * Numeric operator support for numeric arrays.
 *
 * @author dramage
 */
class RichArrayVector[V:ClassManifest](override val repr : Array[V])
extends MutableWrappedColOps[Array[V]] {
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
extends MutableWrappedColOps[SparseArray[V]];

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

/**
 * Numeric operator support for tuples of numeric values.
 *
 * @author dramage
 */
class RichTuple2[@specialized A, @specialized B]
(override val repr : (A,B))
extends NumericOps[(A,B)];

/**
 * Numeric operator support for tuples of numeric values.
 *
 * @author dramage
 */
class RichTuple3[@specialized A, @specialized B, @specialized C]
(override val repr : (A,B,C))
extends NumericOps[(A,B,C)];

/**
 * Numeric operator support for tuples of numeric values.
 *
 * @author dramage
 */
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

/**
 * Implicit promotions of built-in scala types to enriched numerically valued
 * equivalents.  After importing the members of this class, you can directly
 * do math on all tuples, arrays, and maps.
 *
 * @author dramage
 */
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
