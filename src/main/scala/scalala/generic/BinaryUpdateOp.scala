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

import scalala.collection.domain.DomainException;

/**
 * Operation that updates A using B.
 * 
 * @author dramage
 */
trait BinaryUpdateOp[-A,-B] {
  def apply(a : A, b : B) : Unit;
}

/** Base class for BinaryUpdateOp on a pair of arrays. @author dramage */
class ArrayArrayUpdateOp[V1,V2](implicit op : BinaryOp[V1,V2,V1])
extends BinaryUpdateOp[Array[V1],Array[V2]] {
  def apply(a : Array[V1], b : Array[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    var i = 0;
    while (i < a.length) {
      a(i) = op(a(i),b(i));
      i += 1;
    }
  }
}

/** Base class for BinaryUpdateOp on a pair of arrays. @author dramage */
class ArrayScalarUpdateOp[V1,B](implicit op : BinaryOp[V1,B,V1], sb : Scalar[B])
extends BinaryUpdateOp[Array[V1],B] {
  def apply(a : Array[V1], b : B) = {
    var i = 0;
    while (i < a.length) {
      a(i) = op(a(i),b);
      i += 1;
    }
  }
}

/** Mutation delegate for A := B. @author dramage */
trait CanAssignInto[-A,-B] extends BinaryUpdateOp[A,B];

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
      i;
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
      var i = 0;
      while (i < a.length) {
        a(i) = b(i);
        i += 1;
      }
      i;
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
}

/** Mutation delegate for A :+= B. @author dramage */
trait CanAddInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanAddInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanAdd[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanAdd[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanAddInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanAdd[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanAdd[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanAddInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :-= B. @author dramage */
trait CanSubInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanSubInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanSub[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanSub[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanSubInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanSub[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanSub[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanSubInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :*= B. @author dramage */
trait CanMulInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanMulInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanMul[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanMul[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanMulInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanMul[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanMul[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanMulInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :/= B. @author dramage */
trait CanDivInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanDivInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanDiv[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanDiv[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanDivInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanDiv[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanDiv[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanDivInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :/= B. @author dramage */
trait CanModInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanModInto {
  implicit def mkIntoArrayArray[V1,V2](implicit op : CanMod[V1,V2,V1])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : CanMod[V1,V2,V1])
  extends ArrayArrayUpdateOp[V1,V2] with CanModInto[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];

  implicit def mkIntoArrayScalar[V1,B](implicit op : CanMod[V1,B,V1], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : CanMod[V1,B,V1], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with CanModInto[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
}

/** Mutation delegate for A :^= B. @author dramage */
trait CanPowInto[-A,-B] extends BinaryUpdateOp[A,B];
