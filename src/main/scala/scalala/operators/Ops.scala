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

  def unary_-[TT>:This,That](implicit op : UnaryOp[TT,OpNeg,That]) = op(repr);

  def :+[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpAdd,That]) = op(repr,b);

  def :-[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpSub,That]) = op(repr,b);

  def :*[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMul,That]) = op(repr,b);

  def :/[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpDiv,That]) = op(repr,b);

  def :%[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMod,That]) = op(repr,b);

  def :^[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpPow,That]) = op(repr,b);

  def :<[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpLT,That]) = op(repr,b);

  def :<=[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpLTE,That]) = op(repr,b);

  def :>[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpGT,That]) = op(repr,b);

  def :>=[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpGTE,That]) = op(repr,b);

  def :==[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpEq,That]) = op(repr,b);

  def :!=[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpNe,That]) = op(repr,b);

  //
  // Operator aliases
  //

  /** Alias for :+(b) for all b. */
  final def +[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpAdd,That]) =
    this.:+(b);

  /** Alias for :-(b) for all b. */
  final def -[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpSub,That]) =
    this.:-(b);

  /** Alias for :*(b) when b is a scalar. */
  final def *[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMul,That], sb : Scalar[B]) =
    this.:*(b);

  /** Alias for :/(b) when b is a scalar. */
  final def /[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpDiv,That], sb : Scalar[B]) =
    this.:/(b);

  /** Alias for :%(b) when b is a scalar. */
  final def %[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMod,That], sb : Scalar[B]) =
    this.:%(b);

  /** Alias for :<(b) for all b. */
  final def <[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpLT,That]) =
    this.:<(b);

  /** Alias for :<=(b) for all b. */
  final def <=[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpLTE,That]) =
    this.:<=(b);

  /** Alias for :>(b) for all b. */
  final def >[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpGT,That]) =
    this.:>(b);

  /** Alias for :>=(b) for all b. */
  final def >=[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpGTE,That]) =
    this.:>=(b);
}

/**
 * Operators for mutable numeric collections.
 *
 * @author dramage
 */
trait MutableNumericOps[+This] extends NumericOps[This] {
  def repr : This;

  def :=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpSet]) : This = {
    op(repr,b);
    return repr;
  }
  
  def :+=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpAdd]) : This = {
    op(repr,b);
    return repr;
  }

  def :-=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpSub]) : This = {
    op(repr,b);
    return repr;
  }

  def :*=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMul]) : This = {
    op(repr,b);
    return repr;
  }

  def :/=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpDiv]) : This = {
    op(repr,b);
    return repr;
  }

  def :%=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMod]) : This = {
    op(repr,b);
    return repr;
  }

  def :^=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpPow]) : This = {
    op(repr,b);
    return repr;
  }

  /** Alias for :+=(b) for all b. */
  final def +=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpAdd]) =
    this.:+=[TT,B](b);

  /** Alias for :-=(b) for all b. */
  final def -=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpSub]) =
    this.:-=[TT,B](b);

  /** Alias for :*=(b) when b is a scalar. */
  final def *=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMul], sb : Scalar[B]) =
    this.:*=[TT,B](b);

  /** Alias for :/=(b) when b is a scalar. */
  final def /=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpDiv], sb : Scalar[B]) =
    this.:/=[TT,B](b);

  /** Alias for :%=(b) when b is a scalar. */
  final def %=[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMod], sb : Scalar[B]) =
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
  def *[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMulColVectorBy,That]) : That =
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
  def *[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMulRowVectorBy,That]) : That =
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
  def *[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMulMatrixBy,That]) =
    op.apply(repr,b);

  def \[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpSolveMatrixBy,That]) =
    op.apply(repr,b);

  def t[TT>:This,That](implicit op : CanTranspose[TT,That]) =
    op.apply(repr);
}

// /**
//  * Specialized column and row tensor ops for wrapped data structures that
//  * cannot directly inherit and distinguish between rows and columns in the
//  * type system.  This class assumes unwrapped objects are columns and
//  * wrapped ones are rows.
//  *
//  * @author dramage
//  */
// trait WrappedColOps[+This] extends NumericOps[This] {
//   def *[TT>:This,B,That](b : WrappedRowOps[B])(implicit op : BinaryOp[TT,B,OpMulColVectorBy,That]) : That =
//     op(repr,b.column);
// 
//   def t : WrappedRowOps[This] = WrappedRowOps(repr);
// }
// 
// /**
//  * A wrapped column tensor whose underyling collection is mutable.  This trait
//  * should be used instead of mixing in "WrappedColOps with
//  * MutableNumeriCollectionOps" directly, because .t needs to return an instance
//  * of MutableRowOps insetad of RowOps.
//  *
//  * @author dramage
//  */
// trait MutableWrappedColOps[+This] extends WrappedColOps[This] with MutableNumericOps[This] {
//   override def t : MutableWrappedRowOps[This] = MutableWrappedRowOps(repr);
// }
// 
// /**
//  * Secialized NumericOps with shaped operations taking A is a row.
//  * Note that there is an inherent asymmetry between WrappedColumnTensorOps and
//  * WrappedRowOps: because tensors are assumed to be columns until reshaped
//  * (e.g. by calling .t), that class extends NumericOps[A].  This
//  * class, by contrast, must preserve the fact that the base numeric operations
//  * like plus must honor the row shape, and that the return result should also
//  * be a row.  Hence this class extends NumericOps[WrappedRowOps[A]]
//  * and provides implicit magic in the companion object to wrap the
//  * corresponding construction delegates.
//  *
//  * @author dramage
//  */
// trait WrappedRowOps[+This] extends NumericOps[WrappedRowOps[This]] {
//   override def repr : WrappedRowOps[This] = this;
// 
//   def column : This;
// 
//   def *[TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMulRowVectorBy,That]) : That =
//     op(this.column,b);
// 
//   /** The transpose returns the underlying value, which assumed to be a column. */
//   def t : This = column;
// }
// 
// object WrappedRowOps {
//   def apply[This](v : This) : WrappedRowOps[This] =
//     new WrappedRowOps[This] { override def column = v; }
// 
//   class WrappedRowBinaryOp[A,-B,Op<:OpType,+That](implicit op : BinaryOp[A,B,Op,That])
//   extends BinaryOp[WrappedRowOps[A],WrappedRowOps[B],Op,WrappedRowOps[That]] {
//     override def apply(a : WrappedRowOps[A], b : WrappedRowOps[B]) =
//       WrappedRowOps(op(a.column,b.column));
//   }
// 
//   implicit def unwrap[A,B,Op<:OpType,That](implicit op : BinaryOp[A,B,Op,That])
//   = new WrappedRowBinaryOp[A,B,Op,That];
// }
// 
// /**
//  * Specialized WrappedRowOps support for WrappedRows that have mutable
//  * underlying collections.
//  *
//  * @author dramage
//  */
// trait MutableWrappedRowOps[+This]
// extends WrappedRowOps[This] with MutableNumericOps[WrappedRowOps[This]];
// 
// object MutableWrappedRowOps {
//   def apply[This](v : This) : MutableWrappedRowOps[This] =
//     new MutableWrappedRowOps[This] { override def column = v; }
// 
//   class MutableWrappedRowBinaryOp[A,-B,Op<:OpType,+That](implicit op : BinaryOp[A,B,Op,That])
//   extends BinaryOp[MutableWrappedRowOps[A],WrappedRowOps[B],Op,MutableWrappedRowOps[That]] {
//     override def apply(a : MutableWrappedRowOps[A], b : WrappedRowOps[B]) =
//       MutableWrappedRowOps(op(a.column,b.column));
//   }
// 
//   class MutableWrappedRowBinaryUpdateOp[A,-B,Op<:OpType](implicit op : BinaryUpdateOp[A,B,Op])
//   extends BinaryUpdateOp[MutableWrappedRowOps[A],WrappedRowOps[B],Op] {
//     override def apply(a : MutableWrappedRowOps[A], b : WrappedRowOps[B]) =
//       op(a.column, b.column);
//   }
// 
//   implicit def unwrapBinaryOp[A,B,Op<:OpType,That](implicit op : BinaryOp[A,B,Op,That])
//   = new MutableWrappedRowBinaryOp[A,B,Op,That];
// 
//   implicit def unwrapBinaryUpdateOp[A,B,Op<:OpType](implicit op : BinaryUpdateOp[A,B,Op])
//   = new MutableWrappedRowBinaryUpdateOp[A,B,Op];
// }

/**
 * Numeric operator support for numeric arrays.
 *
 * @author dramage
 */
class RichArrayVector[V:ClassManifest](override val repr : Array[V])
extends MutableNumericOps[Array[V]] {
//extends MutableWrappedColOps[Array[V]] {

  /** Returns a vector view of this array. */
  def asVector(implicit s : Scalar[V]) = new scalala.tensor.dense.DenseVectorCol(repr);
}

/**
 * Numeric operator support for Array[Array] matrix.
 * 
 * @author dramage
 */
class RichArrayMatrix[V:ClassManifest](override val repr : Array[Array[V]])
extends MutableNumericOps[Array[Array[V]]] {
//extends MatrixOps[Array[Array[V]]] {

  /** Returns a matrix view of this array. */
  def asMatrix(implicit s : Scalar[V]) = new scalala.tensor.dense.ArrayArrayMatrix(repr);
}

/**
 * Numeric operator support for SparseArray vector.
 *
 * @author dramage
 */
class RichSparseArrayVector[V:ClassManifest:DefaultArrayValue](override val repr : SparseArray[V])
extends MutableNumericOps[SparseArray[V]] {
//extends MutableWrappedColOps[SparseArray[V]] {

  /** Returns a vector view of this sparse array. */
  def asVector(implicit s : Scalar[V]) = new scalala.tensor.sparse.SparseVectorCol(repr);
}

/**
 * Numeric operator support for solo scalars.  Note: raw + is ambiguous
 * with the any2String implicit that comes built in with scala and is used
   for string concatenation.  Use :+ instead.
 *
 * @author dramage
 */
class RichScalar[@specialized V:Scalar](override val repr : V)
extends NumericOps[V] {
  /** This is a scalar, so alias for :*(b) for all b. */
  final def *[B,That](b : B)(implicit op : BinaryOp[V,B,OpMul,That]) =
    this.:*(b);

  /** This is a scalar, so alias for :/(b) for all b. */
  final def /[B,That](b : B)(implicit op : BinaryOp[V,B,OpDiv,That]) =
    this.:/(b);

  /** This is a scalar, so alias for :%(b) for all b. */
  final def %[B,That](b : B)(implicit op : BinaryOp[V,B,OpMod,That]) =
    this.:%(b);
}

/**
 * Numeric operator support for tuples of numeric values.
 *
 * @author dramage
 */
class RichTuple2[@specialized A, @specialized B]
(override val repr : (A,B))
extends MutableNumericOps[(A,B)];

/**
 * Numeric operator support for tuples of numeric values.
 *
 * @author dramage
 */
class RichTuple3[@specialized A, @specialized B, @specialized C]
(override val repr : (A,B,C))
extends MutableNumericOps[(A,B,C)];

/**
 * Numeric operator support for tuples of numeric values.
 *
 * @author dramage
 */
class RichTuple4[@specialized A, @specialized B, @specialized C, @specialized D]
(override val repr : (A,B,C,D))
extends MutableNumericOps[(A,B,C,D)];

/**
 * Adds rich math operators to a map.  Adds mutable as well as
 * immutable operators because inner data structures may be mutable.
 *
 * @author dramage
 */
class RichMap[M<:scala.collection.Map[_,_]](override val repr : M)
extends MutableNumericOps[M];

/**
 * Adds rich math operators to a seq.  Adds mutable as well as
 * immutable operators because inner data structures may be mutable.
 *
 * @author dramage
 */
class RichSeq[S<:scala.collection.Seq[_]](override val repr : S)
extends MutableNumericOps[S] {
//  TODO: add me when we can reliably get the value type from the collection
//
//  /** Constructs a new vector view of this sequence. */
//  def toVector(implicit s : Scalar[V]) = {
//    implicit val mf = s.manifest;
//    new RichArray(repr.toArray).asVector;
//  }
//
//  /**
//   * Constructs a new matrix view of this sequence.
//   * If only one of rows or cols is specified, infers the other.
//   */
//  def toMatrix(rows : Int = -1, cols : Int = -1)(implicit s : Scalar[V]) = {
//    implicit val mf = s.manifest;
//    new RichArray(repr.toArray).asMatrix(rows, cols);
//  }
}

/**
 * Adds rich math operators to an array.
 *
 * @author dramage
 */
class RichArray[@specialized V](override val repr : Array[V])
extends MutableNumericOps[Array[V]] {
  /** Constructs a view of this array as a column vector. */
  def asVector(implicit s : Scalar[V]) =
    new scalala.tensor.dense.DenseVectorCol(repr);
    
  /**
   * Constructs a view of this array as a matrix of the given dimensions.
   * If only one of rows or cols is specified, infers the other.
   */
  def asMatrix(rows : Int = -1, cols : Int = -1)(implicit s : Scalar[V]) = {
    require(rows > 0 || cols > 0, "Must specify either rows or cols to view as matrix.");
    val r = if (rows >= 0) rows else {
      require(repr.length % cols == 0, "Input length does not evenly divide by requested number of cols");
     repr.length / cols;
    };
    
    val c = if (cols >= 0) cols else {
      require(repr.length % rows == 0, "Input length does not evenly divide by requested number of rows");
     repr.length / rows;
    };
    
    new scalala.tensor.dense.DenseMatrix(r, c, repr);
  }
}

/**
 * Adds rich math operators to functions.
 *
 * @author dramage
 */
class RichFunction2[@specialized A, @specialized B](override val repr : A=>B)
extends NumericOps[A=>B] {
  def :@[VV,That](values : VV)(implicit bf : scala.collection.generic.CanBuildFrom[VV,B,That], vv : VV => TraversableOnce[A]) = {
    val rv = bf(values);
    for (v <- values) rv += repr(v);
    rv.result;
  }
}

/**
 * Implicit promotions of built-in scala types to enriched numerically valued
 * equivalents.  After importing the members of this class, you can directly
 * do math on all tuples, arrays, and maps.
 *
 * @author dramage
 */
object Implicits {
  implicit def richScalar[@specialized V:Scalar](value : V) =
    new RichScalar(value);

  implicit def richTuple2[@specialized A, @specialized B](value : (A,B)) =
    new RichTuple2(value);

  implicit def richTuple3[@specialized A, @specialized B, @specialized C](value : (A,B,C)) =
    new RichTuple3(value);

  implicit def richTuple4[@specialized A, @specialized B, @specialized C, @specialized D](value : (A,B,C,D)) =
    new RichTuple4(value);
  
  implicit def richMap[M<:scala.collection.Map[_,_]](map : M) = new RichMap(map);
  
  implicit def richSeq[S<:scala.collection.Seq[_]](seq : S) = new RichSeq(seq);
  
  implicit def richArray[@specialized V](arr : Array[V]) = new RichArray(arr);
}

