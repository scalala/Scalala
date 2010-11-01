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

/**
 * Construction delegate for outer (dot) product A * B.
 *
 * @author dramage
 */
trait CanMulColumnBy[A,-B,+That] extends BinaryOp[A,B,That];

object CanMulColumnBy {
  implicit def CanMulColumnByArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV])
  = new CanMulColumnByArrayArray[V1,V2,RV];

  class CanMulColumnByArrayArray[V1,V2,RV](implicit m : ClassManifest[RV], mul : CanMul[V1,V2,RV])
  extends CanMulColumnBy[Array[V1],Array[V2],Array[Array[RV]]] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      Array.tabulate(a.length,a.length)((i,j) => mul(a(i),b(j)));
    }
  }

  implicit object CanMulColumnByArrayArrayII extends CanMulColumnByArrayArray[Int,Int,Int];
  implicit object CanMulColumnByArrayArrayDD extends CanMulColumnByArrayArray[Double,Double,Double];
  implicit object CanMulColumnByArrayArrayDI extends CanMulColumnByArrayArray[Double,Int,Double];
  implicit object CanMulColumnByArrayArrayID extends CanMulColumnByArrayArray[Int,Double,Double];
}
