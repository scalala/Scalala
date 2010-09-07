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
import scalala.collection.domain.DomainException;

trait NumericCollectionOps[+This] {
  def repr : This;

  def unary_-[That](implicit op : CanNeg[This,That]) : That = op(repr);

  def :+[B,That](b : B)(implicit op : CanAdd[This,B,That]) = op(repr,b);

  def :-[B,That](b : B)(implicit op : CanSub[This,B,That]) = op(repr,b);

  def :*[B,That](b : B)(implicit op : CanMul[This,B,That]) = op(repr,b);

  def :/[B,That](b : B)(implicit op : CanDiv[This,B,That]) = op(repr,b);

  def :%[B,That](b : B)(implicit op : CanMod[This,B,That]) = op(repr,b);

  def :^[B,That](b : B)(implicit op : CanPow[This,B,That]) = op(repr,b);

  def :<[B,That](b : B)(implicit op : CanLT[This,B,That]) = op(repr,b);

  def :<=[B,That](b : B)(implicit op : CanLTE[This,B,That]) = op(repr,b);

  def :>[B,That](b : B)(implicit op : CanGT[This,B,That]) = op(repr,b);

  def :>=[B,That](b : B)(implicit op : CanGTE[This,B,That]) = op(repr,b);

  def :==[B,That](b : B)(implicit op : CanEq[This,B,That]) = op(repr,b);

  def :!=[B,That](b : B)(implicit op : CanNe[This,B,That]) = op(repr,b);

  /** Final alias for this.:+(b) */
  final def +[B,That](b : B)(implicit op : CanAdd[This,B,That]) = this.:+(b);

  /** Final alias for this.:-(b) */
  final def -[B,That](b : B)(implicit op : CanSub[This,B,That]) = this.:-(b);
}


trait MutableNumericCollectionOps[+This] extends NumericCollectionOps[This] {
  def repr : This;

  def :=[B](b : B)(implicit op : CanAssignInto[This,B]) = op(repr,b);

  def :+=[B](b : B)(implicit op : CanAddInto[This,B]) = op(repr,b);

  def :-=[B](b : B)(implicit op : CanSubInto[This,B]) = op(repr,b);

  def :*=[B](b : B)(implicit op : CanMulInto[This,B]) = op(repr,b);

  def :/=[B](b : B)(implicit op : CanDivInto[This,B]) = op(repr,b);

  def :%=[B](b : B)(implicit op : CanModInto[This,B]) = op(repr,b);

  def :^=[B](b : B)(implicit op : CanPowInto[This,B]) = op(repr,b);

  /** Final alias for this.:+=(b) */
  final def +=[B](b : B)(implicit op : CanAddInto[This,B]) = this.:+=(b);

  /** Final alias for this.:-=(b) */
  final def -=[B](b : B)(implicit op : CanSubInto[This,B]) = this.:-=(b);
}

//
// Shaped operations
// 


/** Construction delegate for outer (dot) product A * B. @author dramage */
trait CanMulOuter[-A,-B,+That] extends BinaryOp[A,B,That];

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
trait CanMulMatrixBy[-A,-B,+That] extends BinaryOp[A,B,That];

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
 * Secialized NumericCollectionOps with shaped operations taking A is a column.
 * Note that columns are the default shape, so this trait simply extends
 * NumericCollectionOps[A].
 *
 * @author dramage
 */
trait ColumnTensorOps[+A] extends NumericCollectionOps[A] {
  def *[B,RV](b : RowTensorOps[B])(implicit op : CanMulOuter[A,B,RV]) : RV =
    op(repr,b.column);

  def t : RowTensorOps[A] = RowTensorOps(repr);
}

/**
 * A column tensor whose underyling collection is mutable.  This trait
 * should be used instead of mixing in "ColumnTensorOps with
 * MutableNumeriCollectionOps" directly, because .t needs to return an instance
 * of MutableRowTensorOps insetad of RowTensorOps.
 *
 * @author dramage
 */
trait MutableColumnTensorOps[+A] extends ColumnTensorOps[A] with MutableNumericCollectionOps[A] {
  override def t : MutableRowTensorOps[A] = MutableRowTensorOps(repr);
}


/** For A*B where A is a row vector. @author dramage */
trait CanMulRowBy[-A,-B,+That] extends BinaryOp[A,B,That];

object CanMulRowBy {
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
 * Secialized NumericCollectionOps with shaped operations taking A is a row.
 * Note that there is an inherent asymmetry between ColumnTensorOps and
 * RowTensorOps: because tensors are assumed to be columns until reshaped
 * (e.g. by calling .t), that class extends NumericCollectionOps[A].  This
 * class, by contrast, must preserve the fact that the base numeric operations
 * like plus must honor the row shape, and that the return result should also
 * be a row.  Hence this class extends NumericCollectionOps[RowTensorOps[A]]
 * and provides implicit magic in the companion object to wrap the
 * corresponding construction delegates.
 *
 * @author dramage
 */
trait RowTensorOps[+A] extends NumericCollectionOps[RowTensorOps[A]] {
  override def repr : RowTensorOps[A] = this;

  def column : A;

  def *[B,RV](b : B)(implicit op : CanMulRowBy[A,B,RV]) : RV =
    op(this.column,b);

  /** The transpose returns the underlying value, which assumed to be a column. */
  def t : A = column;
}

object RowTensorOps {
  def apply[This](v : This) : RowTensorOps[This] =
    new RowTensorOps[This] { override def column = v; }

  class RowBinaryOp[-A,-B,+That](implicit op : BinaryOp[A,B,That])
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
trait MutableRowTensorOps[+A]
extends RowTensorOps[A] with MutableNumericCollectionOps[RowTensorOps[A]] {
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
trait MatrixOps[+A] extends NumericCollectionOps[A] {
  def *[B,That](b : B)(implicit op : CanMulMatrixBy[A,B,That]) =
    op.apply(repr,b);
}

// TODO: default DomainMap implementation of all operations should
// use reflection to find a more specialized runtime method.

//
// Enriched types
// 

/** Numeric operator support for numeric arrays. @author dramage */
class RichArrayVector[V:ClassManifest](override val repr : Array[V])
extends MutableColumnTensorOps[Array[V]];

/** Numeric operator support for Array[Array] matrix. @author dramage */
class RichArrayMatrix[V:ClassManifest](override val repr : Array[Array[V]])
extends MatrixOps[Array[Array[V]]];

/**
 * Numeric operator support for solo scalars.  Note: we do not support
 * raw "+" to avoid ambiguity with the any2String implicit that comes built
 * in with scala and is used for string concatenation.  Use :+ instead.
 * Similarly, use ":-" instead of "-", which we exclude for consistency.
 *
 * @author dramage
 */
class RichScalar[@specialized(Int,Long,Float,Double) A:Scalar](val scalar : A) {
  /** Commutative: defer to scalar + b. */
  def :+[B,That](b : B)(implicit op : CanAdd[B,A,That]) = op(b,scalar);

  /** Not commutative: need special implementation of scalar - b. */
  def :-[B,That](b : B)(implicit op : CanSub[A,B,That]) = op(scalar,b);

  /** Commutative: defer to scalar * b. */
  def :*[B,That](b : B)(implicit op : CanMul[B,A,That]) = op(b,scalar);

  /** Not commutative: need special implementation of scala / b. */
  def :/[B,That](b : B)(implicit op : CanDiv[A,B,That]) = op(scalar,b);

  /** Not commutative: need special implementation of scala % b. */
  def :%[B,That](b : B)(implicit op : CanMod[A,B,That]) = op(scalar,b);

  // TODO: Add :^
}

class RichTuple2[@specialized A, @specialized B]
(override val repr : (A,B))
extends NumericCollectionOps[(A,B)];

class RichTuple3[@specialized A, @specialized B, @specialized C]
(override val repr : (A,B,C))
extends NumericCollectionOps[(A,B,C)];

class RichTuple4[@specialized A, @specialized B, @specialized C, @specialized D]
(override val repr : (A,B,C,D))
extends NumericCollectionOps[(A,B,C,D)];


/** Numeric operator support for scala maps. @athor dramage */
class RichMap[K,V](override val repr : Map[K,V])
extends NumericCollectionOps[Map[K,V]];

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
    println((x + y) mkString(" "));
    println((x - y) mkString(" "));

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

    println(Map("a"->1,"b"->2) + Map("a"->1,"b"->2));

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
