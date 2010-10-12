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
import scalala.tensor.Scalar;
import scalala.tensor.domain.DomainException;
import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

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


/** Construction delegate for outer (dot) product A * B. @author dramage */
trait CanMulOuter[A,-B,+That] extends BinaryOp[A,B,That];

object CanMulOuter {
  implicit def CanMulOuterArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV])
  = new CanMulOuterArrayArray[V1,V2,RV];

  class CanMulOuterArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV])
  extends CanMulOuter[Array[V1],Array[V2],Array[Array[RV]]] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      Array.tabulate(a.length,a.length)((i,j) => mul(a(i),b(j)));
    }
  }

  implicit object CanMulOuterArrayArrayII extends CanMulOuterArrayArray[Int,Int,Int];
  implicit object CanMulOuterArrayArrayDD extends CanMulOuterArrayArray[Double,Double,Double];
  implicit object CanMulOuterArrayArrayDI extends CanMulOuterArrayArray[Double,Int,Double];
  implicit object CanMulOuterArrayArrayID extends CanMulOuterArrayArray[Int,Double,Double];
}

/** For A*B where A is a matrix. @author dramage */
trait CanMulMatrixBy[A,-B,+That] extends BinaryOp[A,B,That];

object CanMulMatrixBy {
  implicit def CanMulArrayMatrixByArray[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV]) =
     new CanMulArrayMatrixByArray[V1,V2,RV];

  class CanMulArrayMatrixByArray[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulMatrixBy[Array[Array[V1]],Array[V2],Array[RV]] {
    override def apply(a : Array[Array[V1]], b : Array[V2]) = {
      val rv = Array.fill(a.length)(srv.zero);
      var i = 0;
      while (i < rv.length) {
        val row = a(i);
        if (row.length != b.length) {
          throw new DomainException(this.getClass.getSimpleName + ": row "+i+" mismatched length: "+row.length+ " vs "+b.length+" entries");
        }
        var j = 0;
        while (j < b.length) {
          rv(i) = add(rv(i), mul(row(j), b(j)));
          j += 1;
        }
        i += 1;
      }
      rv;
    }
  }

  implicit object CanMulArrayMatrixByArrayII extends CanMulArrayMatrixByArray[Int,Int,Int];
  implicit object CanMulArrayMatrixByArrayDD extends CanMulArrayMatrixByArray[Double,Double,Double];
  implicit object CanMulArrayMatrixByArrayDI extends CanMulArrayMatrixByArray[Double,Int,Double];
  implicit object CanMulArrayMatrixByArrayID extends CanMulArrayMatrixByArray[Int,Double,Double];

  
  implicit def CanMulArrayMatrixByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV]) =
     new CanMulArrayMatrixByArrayMatrix[V1,V2,RV];

  class CanMulArrayMatrixByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulMatrixBy[Array[Array[V1]],Array[Array[V2]],Array[Array[RV]]] {
    override def apply(a : Array[Array[V1]], b : Array[Array[V2]]) = {
      val numRows = a.length;
      val numCols = b(0).length;
      val numInner = b.length;

      Array.tabulate(numRows, numCols){(i,j) =>
        var rv = srv.zero;
        var k = 0;
        while (k < numInner) {
          rv = add(rv, mul(a(i)(k),b(k)(j)));
          k += 1;
        }
        rv;
      }
    }
  }

  implicit object CanMulArrayMatrixByArrayMatrixII extends CanMulArrayMatrixByArrayMatrix[Int,Int,Int];
  implicit object CanMulArrayMatrixByArrayMatrixDD extends CanMulArrayMatrixByArrayMatrix[Double,Double,Double];
  implicit object CanMulArrayMatrixByArrayMatrixDI extends CanMulArrayMatrixByArrayMatrix[Double,Int,Double];
  implicit object CanMulArrayMatrixByArrayMatrixID extends CanMulArrayMatrixByArrayMatrix[Int,Double,Double];
}

/**
 * Secialized NumericOps with shaped operations taking A is a column.
 * Note that columns are the default shape, so this trait simply extends
 * NumericOps[A].
 *
 * @author dramage
 */
trait ColumnTensorOps[+This] extends NumericOps[This] {
  def *[TT>:This,B,RV](b : RowTensorOps[B])(implicit op : CanMulOuter[TT,B,RV]) : RV =
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


/** For A*B where A is a row vector. @author dramage */
trait CanMulRowBy[A,-B,+That] extends BinaryOp[A,B,That];

object CanMulRowBy {
  //
  // Array * Array
  //

  implicit def CanMulRowArrayByArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  = new CanMulRowArrayByArray[V1,V2,RV];

  /** Array inner product */
  class CanMulRowArrayByArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulRowBy[Array[V1],Array[V2],RV] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var rv = srv.zero;
      var i = 0;
      while (i < a.length) {
        rv = add(rv, mul(a(i),b(i)));
        i += 1;
      }
      rv;
    }
  }

  implicit object CanMulRowArrayByArrayII extends CanMulRowArrayByArray[Int,Int,Int];
  implicit object CanMulRowArrayByArrayDD extends CanMulRowArrayByArray[Double,Double,Double];
  implicit object CanMulRowArrayByArrayDI extends CanMulRowArrayByArray[Double,Int,Double];
  implicit object CanMulRowArrayByArrayID extends CanMulRowArrayByArray[Int,Double,Double];

  //
  // SparseArray * SparseArray
  //

  implicit def CanMulRowSparseArrayBySparseArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  = new CanMulRowSparseArrayBySparseArray[V1,V2,RV];

  /** Array inner product */
  class CanMulRowSparseArrayBySparseArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulRowBy[SparseArray[V1],SparseArray[V2],RV] {
    override def apply(a : SparseArray[V1], b : SparseArray[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var rv = srv.zero;
      var aO = 0;
      var bO = 0;
      while (aO < a.activeLength && bO < b.activeLength) {
        val aI = a.indexAt(aO);
        val bI = b.indexAt(bO);
        if (aI < bI) {
          aO += 1;
        } else if (bI < aI) {
          bO += 1;
        } else {
          rv = add(rv, mul(a.valueAt(aO), b.valueAt(bO)));
          aO += 1;
          bO += 1;
        }
      }
      rv;
    }
  }

  implicit object CanMulRowSparseArrayBySparseArrayII extends CanMulRowSparseArrayBySparseArray[Int,Int,Int];
  implicit object CanMulRowSparseArrayBySparseArrayDD extends CanMulRowSparseArrayBySparseArray[Double,Double,Double];
  implicit object CanMulRowSparseArrayBySparseArrayDI extends CanMulRowSparseArrayBySparseArray[Double,Int,Double];
  implicit object CanMulRowSparseArrayBySparseArrayID extends CanMulRowSparseArrayBySparseArray[Int,Double,Double];

  //
  // SparseArray * Array
  //

  implicit def CanMulRowSparseArrayByArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  = new CanMulRowSparseArrayByArray[V1,V2,RV];

  /** Array inner product */
  class CanMulRowSparseArrayByArray[V1,V2,RV]
  (implicit mul : CanMul[V1,V2,RV], add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulRowBy[SparseArray[V1],Array[V2],RV] {
    override def apply(a : SparseArray[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var rv = srv.zero;
      var o = 0;
      while (o < a.activeLength) {
        rv = add(rv, mul(a.valueAt(o), b(a.indexAt(o))));
        o += 1;
      }
      rv;
    }
  }

  implicit object CanMulRowSparseArrayByArrayII extends CanMulRowSparseArrayByArray[Int,Int,Int];
  implicit object CanMulRowSparseArrayByArrayDD extends CanMulRowSparseArrayByArray[Double,Double,Double];
  implicit object CanMulRowSparseArrayByArrayDI extends CanMulRowSparseArrayByArray[Double,Int,Double];
  implicit object CanMulRowSparseArrayByArrayID extends CanMulRowSparseArrayByArray[Int,Double,Double];

  //
  // Array * Array[Array]
  //

  implicit def CanMulRowArrayByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV]) =
     new CanMulRowArrayByArrayMatrix[V1,V2,RV];

  /** Row array by array matrix */
  class CanMulRowArrayByArrayMatrix[V1,V2,RV]
  (implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV],
   add : CanAdd[RV,RV,RV], srv : Scalar[RV])
  extends CanMulRowBy[Array[V1],Array[Array[V2]],MutableRowTensorOps[Array[RV]]] {
    override def apply(a : Array[V1], b : Array[Array[V2]]) = {
      val rv = Array.fill(b.length)(srv.zero);
      var i = 0;
      while (i < rv.length) {
        var j = 0;
        while (j < a.length) {
          rv(i) = add(rv(i), mul(a(j), b(j)(i)));
          j += 1;
        }
        i += 1;
      }
      MutableRowTensorOps(rv);
    }
  }

  implicit object CanMulRowArrayByArrayMatrixII extends CanMulRowArrayByArrayMatrix[Int,Int,Int];
  implicit object CanMulRowArrayByArrayMatrixDD extends CanMulRowArrayByArrayMatrix[Double,Double,Double];
  implicit object CanMulRowArrayByArrayMatrixDI extends CanMulRowArrayByArrayMatrix[Double,Int,Double];
  implicit object CanMulRowArrayByArrayMatrixID extends CanMulRowArrayByArrayMatrix[Int,Double,Double];
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
 */
trait MutableRowTensorOps[+This]
extends RowTensorOps[This] with MutableNumericOps[RowTensorOps[This]] {
}

object MutableRowTensorOps {
  def apply[This](v : This) : MutableRowTensorOps[This] =
    new MutableRowTensorOps[This] { override def column = v; }

  // TODO: add all wrapped conversions
}

/**
 * Provides matrix-like operations for two dimensional collections.
 * @author dramage
 */
trait MatrixOps[+This] extends NumericOps[This] {
  def *[TT>:This,B,That](b : B)(implicit op : CanMulMatrixBy[TT,B,That]) =
    op.apply(repr,b);
}

//
// Enriched types
// 

/** Numeric operator support for numeric arrays. @author dramage */
class RichArrayVector[V:ClassManifest](override val repr : Array[V])
extends MutableColumnTensorOps[Array[V]] {
  /** Final alias for :+ as a workaround for arrays. */
  final def :+:[B,That](b : B)(implicit op : CanAdd[Array[V],B,That]) = this.:+(b);
}

/** Numeric operator support for Array[Array] matrix. @author dramage */
class RichArrayMatrix[V:ClassManifest](override val repr : Array[Array[V]])
extends MatrixOps[Array[Array[V]]];

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


/** Numeric operator support for scala maps. @athor dramage */
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

object OpsTest {
  import Implicits._;

  def main(args : Array[String]) {
    val x = Array(1,2,3,4);
    val y = Array(-2,-3,-4,-5);
    println(x mkString(" "));
    println(x :/ 2.0 mkString(" "));
    println((-x) mkString(" "));
    println((x :+ 1) mkString(" "));
    println((1 :+ x) mkString(" "));
    println((x :+ y) mkString(" "));
    println((x :- y) mkString(" "));

    x :+= y;  println(x mkString(" "));
    x :-= y;  println(x mkString(" "));
    x :*= y;  println(x mkString(" "));
    x :/= y;  println(x mkString(" "));

    println((Array(1.0,2.0,3.0) :/ Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :/ Array(.5,.5,.5)) mkString(" "));
    println((Array(1.0,2.0,3.0) :* Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :* Array(.5,.5,.5)) mkString(" "));
    println((Array(1.0,2.0,3.0) :% Array(2,2,2)) mkString(" "));
    println((Array(1,2,3) :% Array(.5,.5,.5)) mkString(" "));

    println(Map("a"->1,"b"->2) :+ Map("a"->1,"b"->2));

    // println(DomainMap("a"->1,"b"->2,"c"->3) + DomainMap("a"->1,"b"->2,"c"->3));

    println(x.t :+ y.t);
    println(x.t * y);

    println((x * y.t).map(_.mkString(" ")).mkString("\n"));

    val m : Array[Array[Int]] = x * y.t;
    println((m * Array(3,2,1,0)) mkString(" "));

    m :+= m;
    println(m.map(_.mkString(" ")).mkString("\n"));

    val a = Array(Array(1,2,3),Array(3,0,4));
    val b = Array(Array(-3,7),Array(6,1),Array(2,2));
    println((a * b).map(_.mkString(" ")).mkString("\n"));

    println((1,2) :+ (3,4));
    println((1,2,3) :- (3,2,1));
  }
}
