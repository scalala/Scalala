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
 * Mutation delegate for A := B.
 *
 * @author dramage
 */
trait CanAssignInto[A,-B] extends BinaryUpdateOp[A,B];

object CanAssignInto {
  implicit def mkIntoArrayInnerArrayInner[A,B](implicit op : CanAssignInto[A,B])
  = new IntoArrayInnerArrayInner[A,B];

  class IntoArrayInnerArrayInner[A,B](implicit op : CanAssignInto[A,B])
  extends CanAssignInto[Array[A],Array[B]] {
    override def apply(a : Array[A], b : Array[B]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      var i = 0;
      while (i < a.length) {
        op(a(i), b(i));
        i += 1;
      }
    }
  }

  implicit def mkIntoArrayArray[V1,V2]
  (implicit fn : V2=>V1, s1 : Scalar[V1], s2 : Scalar[V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray
  [@specialized(Int,Long,Float,Double) V1,
   @specialized(Int,Long,Float,Double) V2]
  (implicit fn : V2=>V1, s1 : Scalar[V1], s2 : Scalar[V2])
  extends CanAssignInto[Array[V1],Array[V2]] {
    override def apply(a : Array[V1], b : Array[V2]) = {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      try {
        System.arraycopy(b,0,a,0,a.length);
      } catch {
        case _ =>
          var i = 0;
          while (i < a.length) {
            a(i) = fn(b(i));
            i += 1;
          }
      }
    }
  }

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,V2](implicit fn : V2=>V1, s2 : Scalar[V2])
  = new IntoArrayScalar[V1,V2];

  class IntoArrayScalar
  [@specialized(Int,Long,Float,Double) V1,
   @specialized(Int,Long,Float,Double) V2]
  (implicit fn : V2=>V1, s2 : Scalar[V2])
  extends CanAssignInto[Array[V1],V2] {
    override def apply(a : Array[V1], b : V2) = {
      var i = 0;
      while (i < a.length) {
        a(i) = b;
        i += 1;
      }
      i;
    }
  }

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
  
  implicit def mkIntoSparseArraySparseArray[V1,V2](implicit fn : V2=>V1, s1 : Scalar[V1], s2 : Scalar[V2])
  = new IntoSparseArraySparseArray[V1,V2];

  class IntoSparseArraySparseArray
  [@specialized(Int,Long,Float,Double) V1,
   @specialized(Int,Long,Float,Double) V2]
  (implicit fn : V2=>V1, s1 : Scalar[V1], s2 : Scalar[V2])
  extends CanAssignInto[SparseArray[V1],SparseArray[V2]] {
    override def apply(a : SparseArray[V1], b : SparseArray[V2]) {
      if (a.length != b.length) {
        throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
      }
      if (s1.manifest == s2.manifest) {
        a.set(b.asInstanceOf[SparseArray[V1]]);
      } else {
        a.clear;
        b.foreachActive((k : Int, v : V2) => a(k) = fn(v));
      }
    }
  }

  implicit object IntoSparseArraySparseArrayII extends IntoSparseArraySparseArray[Int,Int];
  implicit object IntoSparseArraySparseArrayDD extends IntoSparseArraySparseArray[Double,Double];
  implicit object IntoSparseArraySparseArrayDI extends IntoSparseArraySparseArray[Double,Int];

  implicit def mkIntoSparseArrayScalar[V1,V2](implicit fn : V2=>V1, sb : Scalar[V2])
  = new IntoSparseArrayScalar[V1,V2];

  class IntoSparseArrayScalar
  [@specialized(Int,Long,Float,Double) V1,
   @specialized(Int,Long,Float,Double) V2]
  (implicit fn : V2 => V1, s2 : Scalar[V2])
  extends CanAssignInto[SparseArray[V1],V2] {
    override def apply(a : SparseArray[V1], b : V2) = {
      val value = fn(b);
      a.transform(v => value);
    }
  }

  implicit object IntoSparseArrayScalarII extends IntoSparseArrayScalar[Int,Int];
  implicit object IntoSparseArrayScalarDI extends IntoSparseArrayScalar[Double,Int];
  implicit object IntoSparseArrayScalarDD extends IntoSparseArrayScalar[Double,Double];
}

