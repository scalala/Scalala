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
package generic;

import scalala.scalar.Scalar;
import scalala.tensor.domain.DomainException;
import scalala.collection.sparse.SparseArray;

import scalala.operators.MutableWrappedRowOps;

/**
 * Construction delegate for A*B where A is a row vector.
 *
 * @author dramage
 */
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
  extends CanMulRowBy[Array[V1],Array[Array[V2]],MutableWrappedRowOps[Array[RV]]] {
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
      MutableWrappedRowOps(rv);
    }
  }

  implicit object CanMulRowArrayByArrayMatrixII extends CanMulRowArrayByArrayMatrix[Int,Int,Int];
  implicit object CanMulRowArrayByArrayMatrixDD extends CanMulRowArrayByArrayMatrix[Double,Double,Double];
  implicit object CanMulRowArrayByArrayMatrixDI extends CanMulRowArrayByArrayMatrix[Double,Int,Double];
  implicit object CanMulRowArrayByArrayMatrixID extends CanMulRowArrayByArrayMatrix[Int,Double,Double];
}
