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
import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

/**
 * Construction delegate for A*B where A is a matrix.
 * 
 * @author dramage
 */
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
