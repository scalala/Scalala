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
import scalala.collection.sparse.{SparseArray};

/**
 * Operation that updates A using B.
 * 
 * @author dramage
 */
trait BinaryUpdateOp[-A,-B] {
  def apply(a : A, b : B) : Unit;
}

/**
 * Base class for BinaryUpdateOp on a pair of arrays.
 *
 * @author dramage
 */
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

/**
 * Base class for BinaryUpdateOp on a pair of arrays.
 *
 * @author dramage
 */
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

/**
 * Base class for updating a SparseArray by another SparseArray.  Considers
 * only non-zeros in the left operand.  Base class of MulInto.
 *
 * @author dramage
 */
class SparseArraySparseArrayUpdateLeftNZOp[V1,V2](implicit op : BinaryOp[V1,V2,V1])
extends BinaryUpdateOp[SparseArray[V1],SparseArray[V2]] {
  def apply(a : SparseArray[V1], b : SparseArray[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    var aO = 0;
    var bO = 0;
    while (aO < a.activeLength && bO < b.activeLength) {
      val aI = a.indexAt(aO);
      val bI = b.indexAt(bO);
      if (aI < bI) {
        a(aI) = op(a.valueAt(aO), b.default);
        aO += 1;
      } else if (bI < aI) {
        bO += 1;
      } else {
        a(aI) = op(a.valueAt(aO), b.valueAt(bO));
        aO += 1;
        bO += 1;
      }
    }
  }
}

/**
 * Base class for updating a SparseArray by another SparseArray.  Considers
 * only non-zeros in the left operand.  Base class of AddInto.
 *
 * @author dramage
 */
class SparseArraySparseArrayUpdateEitherNZOp[V1,V2](implicit op : BinaryOp[V1,V2,V1])
extends BinaryUpdateOp[SparseArray[V1],SparseArray[V2]] {
  def apply(a : SparseArray[V1], b : SparseArray[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    var aO = 0;
    var bO = 0;
    while (aO < a.activeLength && bO < b.activeLength) {
      val aI = a.indexAt(aO);
      val bI = b.indexAt(bO);
      if (aI < bI) {
        a(aI) = op(a.valueAt(aO), b.default);
        aO += 1;
      } else if (bI < aI) {
        a(bI) = op(a.default, b.valueAt(bO));
        bO += 1;
      } else {
        a(aI) = op(a.valueAt(aO), b.valueAt(bO));
        aO += 1;
        bO += 1;
      }
    }
  }
}

/**
 * Base class for updating a SparseArray by another SparseArray.  Considers
 * all values.  Base class of DivInto.
 *
 * @author dramage
 */
class SparseArraySparseArrayUpdateAllOp[V1,V2](implicit op : BinaryOp[V1,V2,V1])
extends BinaryUpdateOp[SparseArray[V1],SparseArray[V2]] {
  def apply(a : SparseArray[V1], b : SparseArray[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }

    /** Optimization: use OuterOp if the default value is itself default */
    if (try { op(a.default, b.default) == a.default } catch { case _ => false; }) {
      (new SparseArraySparseArrayUpdateEitherNZOp[V1,V2]).apply(a,b);
    } else {
      var i = 0;
      while (i < a.length) {
        a(i) = op(a(i),b(i));
        i += 1;
      }
    }
  }
}

class SparseArrayScalarUpdateOp[V1,B](implicit op : BinaryOp[V1,B,V1], sb : Scalar[B])
extends BinaryUpdateOp[SparseArray[V1],B] {
  def apply(a : SparseArray[V1], b : B) =
    a.transform(v => op(v, b));
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
  type Op[A,B] = CanAdd[A,B,A];
  type UpdateOp[A,B] = CanAddInto[A,B];
  type SparseArraySparseArrayBase[A,B] = SparseArraySparseArrayUpdateEitherNZOp[A,B];

  //
  // Below is copy-and-pasted between companion objects
  //

  implicit def mkIntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  extends ArrayArrayUpdateOp[V1,V2] with UpdateOp[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];

  implicit def mkIntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with UpdateOp[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];

  implicit def mkIntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoSparseArraySparseArray;

  class IntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  extends SparseArraySparseArrayBase[V1,V2] with UpdateOp[SparseArray[V1],SparseArray[V2]];

  implicit object IntoSparseArraySparseArrayII extends IntoSparseArraySparseArray[Int,Int];
  implicit object IntoSparseArraySparseArrayDD extends IntoSparseArraySparseArray[Double,Double];
  implicit object IntoSparseArraySparseArrayDI extends IntoSparseArraySparseArray[Double,Int];

  implicit def mkIntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoSparseArrayScalar[V1,B];

  class IntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends SparseArrayScalarUpdateOp[V1,B] with UpdateOp[SparseArray[V1],B];

  implicit object IntoSparseArrayScalarII extends IntoSparseArrayScalar[Int,Int];
  implicit object IntoSparseArrayScalarDI extends IntoSparseArrayScalar[Double,Int];
  implicit object IntoSparseArrayScalarDD extends IntoSparseArrayScalar[Double,Double];
}

/** Mutation delegate for A :-= B. @author dramage */
trait CanSubInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanSubInto {
  type Op[A,B] = CanSub[A,B,A];
  type UpdateOp[A,B] = CanSubInto[A,B];
  type SparseArraySparseArrayBase[A,B] = SparseArraySparseArrayUpdateEitherNZOp[A,B];

  //
  // Below is copy-and-pasted between companion objects
  //

  implicit def mkIntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  extends ArrayArrayUpdateOp[V1,V2] with UpdateOp[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];

  implicit def mkIntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with UpdateOp[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];

  implicit def mkIntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoSparseArraySparseArray;

  class IntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  extends SparseArraySparseArrayBase[V1,V2] with UpdateOp[SparseArray[V1],SparseArray[V2]];

  implicit object IntoSparseArraySparseArrayII extends IntoSparseArraySparseArray[Int,Int];
  implicit object IntoSparseArraySparseArrayDD extends IntoSparseArraySparseArray[Double,Double];
  implicit object IntoSparseArraySparseArrayDI extends IntoSparseArraySparseArray[Double,Int];

  implicit def mkIntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoSparseArrayScalar[V1,B];

  class IntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends SparseArrayScalarUpdateOp[V1,B] with UpdateOp[SparseArray[V1],B];

  implicit object IntoSparseArrayScalarII extends IntoSparseArrayScalar[Int,Int];
  implicit object IntoSparseArrayScalarDI extends IntoSparseArrayScalar[Double,Int];
  implicit object IntoSparseArrayScalarDD extends IntoSparseArrayScalar[Double,Double];
}

/** Mutation delegate for A :*= B. @author dramage */
trait CanMulInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanMulInto {
  type Op[A,B] = CanMul[A,B,A];
  type UpdateOp[A,B] = CanMulInto[A,B];
  type SparseArraySparseArrayBase[A,B] = SparseArraySparseArrayUpdateLeftNZOp[A,B];

  //
  // Below is copy-and-pasted between companion objects
  //

  implicit def mkIntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  extends ArrayArrayUpdateOp[V1,V2] with UpdateOp[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];

  implicit def mkIntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with UpdateOp[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];

  implicit def mkIntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoSparseArraySparseArray;

  class IntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  extends SparseArraySparseArrayBase[V1,V2] with UpdateOp[SparseArray[V1],SparseArray[V2]];

  implicit object IntoSparseArraySparseArrayII extends IntoSparseArraySparseArray[Int,Int];
  implicit object IntoSparseArraySparseArrayDD extends IntoSparseArraySparseArray[Double,Double];
  implicit object IntoSparseArraySparseArrayDI extends IntoSparseArraySparseArray[Double,Int];

  implicit def mkIntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoSparseArrayScalar[V1,B];

  class IntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends SparseArrayScalarUpdateOp[V1,B] with UpdateOp[SparseArray[V1],B];

  implicit object IntoSparseArrayScalarII extends IntoSparseArrayScalar[Int,Int];
  implicit object IntoSparseArrayScalarDI extends IntoSparseArrayScalar[Double,Int];
  implicit object IntoSparseArrayScalarDD extends IntoSparseArrayScalar[Double,Double];
}

/** Mutation delegate for A :/= B. @author dramage */
trait CanDivInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanDivInto {
  type Op[A,B] = CanDiv[A,B,A];
  type UpdateOp[A,B] = CanDivInto[A,B];
  type SparseArraySparseArrayBase[A,B] = SparseArraySparseArrayUpdateAllOp[A,B];

  //
  // Below is copy-and-pasted between companion objects
  //

  implicit def mkIntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  extends ArrayArrayUpdateOp[V1,V2] with UpdateOp[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];

  implicit def mkIntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with UpdateOp[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];

  implicit def mkIntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoSparseArraySparseArray;

  class IntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  extends SparseArraySparseArrayBase[V1,V2] with UpdateOp[SparseArray[V1],SparseArray[V2]];

  implicit object IntoSparseArraySparseArrayII extends IntoSparseArraySparseArray[Int,Int];
  implicit object IntoSparseArraySparseArrayDD extends IntoSparseArraySparseArray[Double,Double];
  implicit object IntoSparseArraySparseArrayDI extends IntoSparseArraySparseArray[Double,Int];

  implicit def mkIntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoSparseArrayScalar[V1,B];

  class IntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends SparseArrayScalarUpdateOp[V1,B] with UpdateOp[SparseArray[V1],B];

  implicit object IntoSparseArrayScalarII extends IntoSparseArrayScalar[Int,Int];
  implicit object IntoSparseArrayScalarDI extends IntoSparseArrayScalar[Double,Int];
  implicit object IntoSparseArrayScalarDD extends IntoSparseArrayScalar[Double,Double];
}

/** Mutation delegate for A :/= B. @author dramage */
trait CanModInto[-A,-B] extends BinaryUpdateOp[A,B];

object CanModInto {
  type Op[A,B] = CanMod[A,B,A];
  type UpdateOp[A,B] = CanModInto[A,B];
  type SparseArraySparseArrayBase[A,B] = SparseArraySparseArrayUpdateAllOp[A,B];

  //
  // Below is copy-and-pasted between companion objects
  //

  implicit def mkIntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  extends ArrayArrayUpdateOp[V1,V2] with UpdateOp[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];

  implicit def mkIntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with UpdateOp[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];

  implicit def mkIntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoSparseArraySparseArray;

  class IntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  extends SparseArraySparseArrayBase[V1,V2] with UpdateOp[SparseArray[V1],SparseArray[V2]];

  implicit object IntoSparseArraySparseArrayII extends IntoSparseArraySparseArray[Int,Int];
  implicit object IntoSparseArraySparseArrayDD extends IntoSparseArraySparseArray[Double,Double];
  implicit object IntoSparseArraySparseArrayDI extends IntoSparseArraySparseArray[Double,Int];

  implicit def mkIntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoSparseArrayScalar[V1,B];

  class IntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends SparseArrayScalarUpdateOp[V1,B] with UpdateOp[SparseArray[V1],B];

  implicit object IntoSparseArrayScalarII extends IntoSparseArrayScalar[Int,Int];
  implicit object IntoSparseArrayScalarDI extends IntoSparseArrayScalar[Double,Int];
  implicit object IntoSparseArrayScalarDD extends IntoSparseArrayScalar[Double,Double];
}

/** Mutation delegate for A :^= B. @author dramage */
trait CanPowInto[-A,-B] extends BinaryUpdateOp[A,B];
