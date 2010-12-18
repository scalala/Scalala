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
import scalala.collection.sparse.{SparseArray};

/**
 * Operation that updates A using B.
 * 
 * @author dramage
 */
trait BinaryUpdateOp[A,@specialized -B] {
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

    // process unpaired remaining from a
    while (aO < a.activeLength) {
      val aI = a.indexAt(aO);
      a(aI) = op(a.valueAt(aO), b.default);
      aO += 1;
    }
  }
}

/**
 * Base class for updating a SparseArray by another SparseArray.  Considers
 * non-zeros in either operand.  Base class of AddInto.
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
    
    // process unpaired remaining from a
    while (aO < a.activeLength) {
      val aI = a.indexAt(aO);
      a(aI) = op(a.valueAt(aO), b.default);
      aO += 1;
    }

    // process unpaired remaining from b
    while (bO < b.activeLength) {
      val bI = b.indexAt(bO);
      a(bI) = op(a.default, b.valueAt(bO));
      bO += 1;
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
