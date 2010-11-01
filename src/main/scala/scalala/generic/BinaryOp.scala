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

//
// Binary operations
//

/**
 * Operation that creates That from A and B.
 * 
 * @author dramage
 */
trait BinaryOp[@specialized A, @specialized -B, +That]
extends ((A,B) => That);


/**
 * Base class for Array (op) Scalar.
 *
 * @author dramage
 */
class ArrayScalarOp[V1,V2,RV]
(implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], s : Scalar[V2])
extends BinaryOp[Array[V1],V2,Array[RV]] {
  override def apply(a : Array[V1], b : V2) = {
    val rv = new Array[RV](a.length);
    var i = 0;
    while (i < rv.length) {
      rv(i) = op(a(i),b);
      i += 1;
    }
    rv;
  }
}

/**
 * Base class for Scalar (op) Array.
 * 
 * @author dramage
 */
class ScalarArrayOp[V1,V2,RV]
(implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], s : Scalar[V1])
extends BinaryOp[V1,Array[V2],Array[RV]] {
  override def apply(a : V1, b : Array[V2]) = {
    val rv = new Array[RV](b.length);
    var i = 0;
    while (i < rv.length) {
      rv(i) = op(a,b(i));
      i += 1;
    }
    rv;
  }
}


/**
 * Base class for BinaryOp on a pair of arrays.
 *
 * @author dramage
 */
class ArrayArrayOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV])
extends BinaryOp[Array[V1],Array[V2],Array[RV]] {
  override def apply(a : Array[V1], b : Array[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new Array[RV](a.length);
    var i = 0;
    while (i < rv.length) {
      rv(i) = op(a(i),b(i));
      i += 1;
    }
    rv;
  }
}


/**
 * Base class for SparseArray (op) Scalar.
 *
 * @author dramage
 */
class SparseArrayScalarOp[V1,V2,RV]
(implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], s : Scalar[V2], dav : DefaultArrayValue[RV])
extends BinaryOp[SparseArray[V1],V2,SparseArray[RV]] {
  override def apply(a : SparseArray[V1], b : V2) =
    a.map(v => op(v, b));
}

/**
 * Base class for Scalar (op) SparseArray.
 * 
 * @author dramage
 */
class ScalarSparseArrayOp[V1,V2,RV]
(implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], s : Scalar[V1], dav : DefaultArrayValue[RV])
extends BinaryOp[V1,SparseArray[V2],SparseArray[RV]] {
  override def apply(a : V1, b : SparseArray[V2]) =
    b.map(v => op(a, v));
}

/**
 * Operation on values that are non-zero in both arguments.  Base class
 * for Mul.
 *
 * @author dramage
 */
class SparseArraySparseArrayBothNZOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[SparseArray[V1],SparseArray[V2],SparseArray[RV]] {
  def apply(a : SparseArray[V1], b : SparseArray[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new SparseArray[RV](a.length);
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
        rv(aI) = op(a.valueAt(aO), b.valueAt(bO));
        aO += 1;
        bO += 1;
      }
    }
    rv;
  }
}

/**
 * Operation on values that are non-zero in at least one argument.  Base class
 * for e.g. Add.
 *
 * @author dramage
 */
class SparseArraySparseArrayEitherNZOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[SparseArray[V1],SparseArray[V2],SparseArray[RV]] {
  def apply(a : SparseArray[V1], b : SparseArray[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new SparseArray[RV](a.length, scala.math.max(a.activeLength,b.activeLength));

    var aO = 0;
    var bO = 0;
    while (aO < a.activeLength && bO < b.activeLength) {
      val aI = a.indexAt(aO);
      val bI = b.indexAt(bO);
      if (aI < bI) {
        rv(aI) = op(a.valueAt(aO), b.default);
        aO += 1;
      } else if (bI < aI) {
        rv(bI) = op(a.default, b.valueAt(bO));
        bO += 1;
      } else {
        rv(aI) = op(a.valueAt(aO), b.valueAt(bO));
        aO += 1;
        bO += 1;
      }
    }

    // process unpaired remaining from a
    while (aO < a.activeLength) {
      val aI = a.indexAt(aO);
      rv(aI) = op(a.valueAt(aO), b.default);
      aO += 1;
    }

    // process unpaired remaining from b
    while (bO < b.activeLength) {
      val bI = b.indexAt(bO);
      rv(bI) = op(a.default, b.valueAt(bO));
      bO += 1;
    }

    rv;
  }
}

/**
 * Operation on all pairs of values, possibly making the output a regular dense
 * Array.
 *
 * @author dramage
 */
class SparseArraySparseArrayAllOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[SparseArray[V1],SparseArray[V2],SparseArray[RV]] {
  def apply(a : SparseArray[V1], b : SparseArray[V2]) : SparseArray[RV] = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new SparseArray[RV](a.length, a.length);

    /** Optimization: use OuterOp if the default value is itself default */
    if (try { op(a.default, b.default) == rv.default } catch { case _ => false; }) {
      return (new SparseArraySparseArrayEitherNZOp[V1,V2,RV]).apply(a,b);
    } else {
      var i = 0;
      while (i < rv.length) {
        rv(i) = op(a(i),b(i));
        i += 1;
      }
      rv;
    }
  }
}

/**
 * Operation on values that are non-zero in both arguments, like mul.
 * Output is sparse.
 *
 * @author dramage
 */
class SparseArrayArrayInnerOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[SparseArray[V1],Array[V2],SparseArray[RV]] {
  def apply(a : SparseArray[V1], b : Array[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new SparseArray[RV](a.length, a.activeLength);
    var o = 0;
    while (o < a.activeLength) {
      val i = a.indexAt(o);
      rv(i) = op(a.valueAt(o), b(i));
      o += 1;
    }
    rv;
  }
}

/**
 * Operation that need to see all values, like add.  Output is dense.
 *
 * @author dramage
 */
class SparseArrayArrayAllToArrayOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[SparseArray[V1],Array[V2],Array[RV]] {
  def apply(a : SparseArray[V1], b : Array[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new Array[RV](a.length);
    var i = 0;
    while (i < a.length) {
      rv(i) = op(a(i), b(i));
      i += 1;
    }
    rv;
  }
}
/**
 * Operation that need to see all values, like add.  Output is dense.
 *
 * @author dramage
 */
class SparseArrayArrayAllToSparseArrayOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[SparseArray[V1],Array[V2],SparseArray[RV]] {
  def apply(a : SparseArray[V1], b : Array[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new SparseArray[RV](a.length, a.activeLength);
    var i = 0;
    while (i < a.length) {
      rv(i) = op(a(i), b(i));
      i += 1;
    }
    rv;
  }
}

/**
 * Operation on values that are non-zero in both arguments, like mul.
 * Output is sparse.
 *
 * @author dramage
 */
class ArraySparseArrayInnerOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[Array[V1],SparseArray[V2],SparseArray[RV]] {
  def apply(a : Array[V1], b : SparseArray[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new SparseArray[RV](b.length, b.activeLength);
    var o = 0;
    while (o < b.activeLength) {
      val i = b.indexAt(o);
      rv(i) = op(a(i), b.valueAt(o));
      o += 1;
    }
    rv;
  }
}

/**
 * Operation that need to see all values, like plus.  Output is dense.
 *
 * @author dramage
 */
class ArraySparseArrayAllOp[V1,V2,RV](implicit m : ClassManifest[RV], op : BinaryOp[V1,V2,RV], dv : DefaultArrayValue[RV])
extends BinaryOp[Array[V1],SparseArray[V2],Array[RV]] {
  def apply(a : Array[V1], b : SparseArray[V2]) = {
    if (a.length != b.length) {
      throw new DomainException(this.getClass.getSimpleName + ": arrays have different lengths");
    }
    val rv = new Array[RV](b.length);
    var i = 0;
    while (i < a.length) {
      rv(i) = op(a(i), b(i));
      i += 1;
    }
    rv;
  }
}

/**
 * Base class for BinaryOp on a pair of scala maps on all values where at
 * least one map contains the given key.
 *
 * @author dramage
 */
class MapMapEitherNonZeroOp[K,V1,V2,RV](implicit op : BinaryOp[V1,V2,RV], sa : Scalar[V1], sb : Scalar[V2])
extends BinaryOp[Map[K,V1],Map[K,V2],Map[K,RV]] {
  def apply(a : Map[K,V1], b : Map[K,V2]) : Map[K,RV] = {
    (a.keySet ++ b.keySet).map(
      k => {
        (k, op(a.getOrElse(k, sa.zero), b.getOrElse(k, sb.zero)));
//        val aHasK = a contains k;
//        val bHasK = b contains k;
//        if (aHasK && bHasK) {
//          (k, op(a(k),b(k)));
//        } else if (aHasK) {
//          (k, promoteL(a(k)));
//        } else /* if (bHasK) */ {
//          (k, promoteR(b(k)));
//        }
      }
    ).toMap;
  }
}

/**
 * Base class for BinaryOp on a pair of scala maps on all values where at
 * both maps contains the given key.
 *
 * @author dramage
 */
class MapMapBothNonZeroOp[K,V1,V2,RV](implicit op : BinaryOp[V1,V2,RV])
extends BinaryOp[Map[K,V1],Map[K,V2],Map[K,RV]] {
  def apply(a : Map[K,V1], b : Map[K,V2]) =
    (a.keySet & b.keySet).map(k => (k,op(a(k),b(k)))).toMap;
}

/**
 * Base of operators on tuples.
 *
 * @author dramage
 */
class Tuple2Tuple2Op[VA1,VA2,VB1,VB2,RV1,RV2]
(implicit op1 : BinaryOp[VA1,VB1,RV1], op2 : BinaryOp[VA2,VB2,RV2])
extends BinaryOp[(VA1,VA2),(VB1,VB2),(RV1,RV2)] {
  def apply(a : (VA1,VA2), b : (VB1,VB2)) =
    (op1(a._1,b._1), op2(a._2,b._2));
}

/**
 * Base of operators on tuples.
 *
 * @author dramage
 */
class Tuple3Tuple3Op[VA1,VA2,VA3,VB1,VB2,VB3,RV1,RV2,RV3]
(implicit op1 : BinaryOp[VA1,VB1,RV1], op2 : BinaryOp[VA2,VB2,RV2],
 op3 : BinaryOp[VA3,VB3,RV3])
extends BinaryOp[(VA1,VA2,VA3),(VB1,VB2,VB3),(RV1,RV2,RV3)] {
  def apply(a : (VA1,VA2,VA3), b : (VB1,VB2,VB3)) =
    (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3));
}

/**
 * Base of operators on tuples.
 *
 * @author dramage
 */
class Tuple4Tuple4Op[VA1,VA2,VA3,VA4,VB1,VB2,VB3,VB4,RV1,RV2,RV3,RV4]
(implicit op1 : BinaryOp[VA1,VB1,RV1], op2 : BinaryOp[VA2,VB2,RV2],
 op3 : BinaryOp[VA3,VB3,RV3], op4 : BinaryOp[VA4,VB4,RV4])
extends BinaryOp[(VA1,VA2,VA3,VA4),(VB1,VB2,VB3,VB4),(RV1,RV2,RV3,RV4)] {
  def apply(a : (VA1,VA2,VA3,VA4), b : (VB1,VB2,VB3,VB4)) =
    (op1(a._1,b._1), op2(a._2,b._2), op3(a._3,b._3), op4(a._4, b._4));
}

/** Construction delegate for A :< B. @author dramage */
trait CanLT[A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :<= B. @author dramage */
trait CanLTE[A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :> B. @author dramage */
trait CanGT[A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :>= B. @author dramage */
trait CanGTE[A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :== B. @author dramage */
trait CanEq[A,-B,+That] extends BinaryOp[A,B,That];

/** Construction delegate for A :!= B. @author dramage */
trait CanNe[A,-B,+That] extends BinaryOp[A,B,That];
