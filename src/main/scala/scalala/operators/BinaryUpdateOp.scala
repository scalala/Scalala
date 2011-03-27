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

import scala.annotation.implicitNotFound;
import scalala.scalar.Scalar;

/**
 * Operation of type O that updates A using B.  Example: A += B.
 *
 * @author dramage
 */
@implicitNotFound(msg="Could not find a way to ${O} ${B} into ${A}")
trait BinaryUpdateOp[@specialized -A, @specialized -B, O<:OpType]
extends ((A,B) => Unit) {
  def opType : O;
}

/**
 * Very low priority implicits.
 *
 * @author dramage
 */
trait BinaryUpdateOpImplicitsLevel0 {

  //
  // Seqs
  //

  /** Set mutable seq with corresponding value from another seq. */  
  implicit def CanSetSeqSeqCast[V1,V2]
  (implicit cast : CanCast[V2,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : CanSet[scala.collection.mutable.Seq[V1], scala.collection.Seq[V2]]
  = new CanSet[scala.collection.mutable.Seq[V1], scala.collection.Seq[V2]] {
    def apply(a : scala.collection.mutable.Seq[V1], b : scala.collection.Seq[V2]) = {
      var i = 0;
      for (v <- b) {
        a(i) = cast(v);
        i += 1;
      }
    }
  }

  /** Set mutable seq with cast scalar. */  
  implicit def OpSetSeqScalarCast[V1,V2]
  (implicit cast : CanCast[V2,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : CanSet[scala.collection.mutable.Seq[V1], V2]
  = new CanSet[scala.collection.mutable.Seq[V1], V2] {
    def apply(a : scala.collection.mutable.Seq[V1], b : V2) = {
      val v = cast(b);
      var i = 0;
      while (i < a.length) {
        a(i) = v;
        i += 1;
      }
    }
  }
  
  //
  // Maps
  //
  
  /** Set mutable map to cast values from another map. */
  implicit def OpSetMapMapCast[K,V1,V2]
  (implicit cast : CanCast[V2,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : CanSet[scala.collection.mutable.Map[K,V1], scala.collection.Map[K,V2]]
  = new CanSet[scala.collection.mutable.Map[K,V1], scala.collection.Map[K,V2]] {
    def apply(a : scala.collection.mutable.Map[K,V1], b : scala.collection.Map[K,V2]) = {
      for (k <- a.keySet) {
        a(k) = cast(b.getOrElse(k, s2.zero));
      }
      for (k <- b.keySet) {
        if (!a.contains(k)) {
          a(k) = cast(b(k));
        }
      }
    }
  }

  /** Set mutable map to cast scalar. */
  implicit def OpSetMapScalarCast[K,V1,V2]
  (implicit cast : CanCast[V2,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : CanSet[scala.collection.mutable.Map[K,V1], V2]
  = new CanSet[scala.collection.mutable.Map[K,V1], V2] {
    def apply(a : scala.collection.mutable.Map[K,V1], b : V2) = {
      val v = cast(b);
      for (k <- a.keySet) {
        a(k) = v;
      }
    }
  }
  
  //
  // Arrays
  //
  
  /** Set array with casted values from another array. */
  implicit def OpSetArrayArrayCast[V1,V2]
  (implicit cast : CanCast[V2,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : CanSet[Array[V1],Array[V2]]
  = new CanSet[Array[V1],Array[V2]] {
    def apply(a : Array[V1], b : Array[V2]) = {
      var i = 0;
      while (i < a.length) {
        a(i) = cast(b(i));
        i += 1;
      }
    }
  }
  
  /** Set array with casted scalar. */
  implicit def OpSetArrayScalarCast[V1,V2]
  (implicit cast : CanCast[V2,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : CanSet[Array[V1],V2]
  = new CanSet[Array[V1],V2] {
    def apply(a : Array[V1], b : V2) = { 
      val v = cast(b);
      var i = 0;
      while (i < a.length) {
        a(i) = v;
        i += 1;
      }
    }
  }
}

/**
 * Low priority implicits.
 *
 * @author dramage
 */
trait BinaryUpdateOpImplicitsLevel1 extends BinaryUpdateOpImplicitsLevel0 {
  
  //
  // Seqs
  //
  
  /** Update mutable seq with corresponding values from another seq. */
  implicit def OpUpdateSeqSeq[V1,V2,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,V1], c : CompatibleShape[V1,V2])
  : BinaryUpdateOp[scala.collection.mutable.Seq[V1], scala.collection.Seq[V2], O]
  = new BinaryUpdateOp[scala.collection.mutable.Seq[V1], scala.collection.Seq[V2], O] {
    def opType = op.opType;
    def apply(a : scala.collection.mutable.Seq[V1], b : scala.collection.Seq[V2]) = {
      require(a.size == b.size, "Inputs must be the same length");
      var i = 0;
      for (v <- b) {
        a(i) = op(a(i),v);
        i += 1;
      }
    }
  }

  /** Set mutable seq with corresponding value from another seq. */  
  implicit def OpSetSeqSeq[V](implicit s : Scalar[V])
  : CanSet[scala.collection.mutable.Seq[V], scala.collection.Seq[V]]
  = new CanSet[scala.collection.mutable.Seq[V], scala.collection.Seq[V]] {
    def apply(a : scala.collection.mutable.Seq[V], b : scala.collection.Seq[V]) = {
      require(a.size == b.size, "Inputs must be the same length");
      var i = 0;
      for (v <- b) {
        a(i) = v;
        i += 1;
      }
    }
  }
  
  /** Update mutable seq with scalar. */
  implicit def OpUpdateSeqScalar[V1,V2,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,V1], s : Scalar[V2])
  : BinaryUpdateOp[scala.collection.mutable.Seq[V1], V2, O]
  = new BinaryUpdateOp[scala.collection.mutable.Seq[V1], V2, O] {
    def opType = op.opType;
    def apply(a : scala.collection.mutable.Seq[V1], b : V2) = {
      var i = 0;
      for (v <- a) {
        a(i) = op(v,b);
        i += 1;
      }
    }
  }

  /** Set mutable seq with scalar. */
  implicit def OpSetSeqScalar[V](implicit s : Scalar[V])
  : CanSet[scala.collection.mutable.Seq[V], V]
  = new CanSet[scala.collection.mutable.Seq[V], V] {
    def apply(a : scala.collection.mutable.Seq[V], b : V) = {
      var i = 0;
      while (i < a.length) {
        a(i) = b;
        i += 1;
      }
    }
  }
  
  
  //
  // Maps
  //
  
  /** Update mutable map with corresponding values from another map. */
  implicit def OpUpdateMapMap[K,V1,V2,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,V1], s1 : Scalar[V1], s2 : Scalar[V2])
  : BinaryUpdateOp[scala.collection.mutable.Map[K,V1], scala.collection.Map[K,V2], O]
  = new BinaryUpdateOp[scala.collection.mutable.Map[K,V1], scala.collection.Map[K,V2], O] {
    def opType = op.opType;
    def apply(a : scala.collection.mutable.Map[K,V1], b : scala.collection.Map[K,V2]) = {
      for (k <- a.keySet) {
        a(k) = op(a(k), b.getOrElse(k,s2.zero));
      }
      for (k <- b.keySet) {
        if (!a.contains(k)) {
          a(k) = op(s1.zero, b(k));
        }
      }
    }
  }

  /** Set contents of mutable map with corresponding values from another map. */
  implicit def OpSetMapMap[K,V](implicit s : Scalar[V])
  : CanSet[scala.collection.mutable.Map[K,V], scala.collection.Map[K,V]]
  = new CanSet[scala.collection.mutable.Map[K,V], scala.collection.Map[K,V]] {
    def apply(a : scala.collection.mutable.Map[K,V], b : scala.collection.Map[K,V]) = {
      for (k <- a.keySet) {
        a(k) = b.getOrElse(k,s.zero);
      }
      for (k <- b.keySet) {
        if (!a.contains(k)) {
          a(k) = b(k);
        }
      }
    }
  }
  
  /** Update mutable map with scalar. */
  implicit def OpUpdateMapScalar[K,V1,V2,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,V1], s : Scalar[V2])
  : BinaryUpdateOp[scala.collection.mutable.Map[K,V1], V2, O]
  = new BinaryUpdateOp[scala.collection.mutable.Map[K,V1], V2, O] {
    def opType = op.opType;
    def apply(a : scala.collection.mutable.Map[K,V1], b : V2) =
      a.transform((k,v) => op(v, b));
  }

  /** Set mutable map to scalar. */
  implicit def OpSetMapScalar[K,V](implicit s : Scalar[V])
  : CanSet[scala.collection.mutable.Map[K,V], V]
  = new CanSet[scala.collection.mutable.Map[K,V], V] {
    def apply(a : scala.collection.mutable.Map[K,V], b : V) = {
      for (k <- a.keySet) {
        a(k) = b;
      }
    }
  }
  
  //
  // Arrays
  //
  
  /** Update array with corresponding values from another array. */
  implicit def OpUpdateArrayArray[V1,V2,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,V1], c : CompatibleShape[V1,V2])
  : BinaryUpdateOp[Array[V1], Array[V2], O]
  = new BinaryUpdateOp[Array[V1], Array[V2], O] {
    def opType = op.opType;
    def apply(a : Array[V1], b : Array[V2]) = {
      require(a.length == b.length, "Inputs must be the same length");
      var i = 0;
      while (i < a.length) {
        a(i) = op(a(i),b(i));
        i += 1;
      }
    }
  }
  
  /** Set array with corresponding values from another array. */
  implicit def OpSetArrayArray[V](implicit s : Scalar[V])
  : CanSet[Array[V],Array[V]]
  = new CanSet[Array[V],Array[V]] {
    def apply(a : Array[V], b : Array[V]) = { 
      require(a.length == b.length, "Inputs must be the same length");
      System.arraycopy(b, 0, a, 0, a.length);
    }
  }
  
  /** Update array with scalar. */
  implicit def OpUpdateArrayScalar[V1,V2,O<:OpType]
  (implicit op : BinaryOp[V1,V2,O,V1], s : Scalar[V2])
  : BinaryUpdateOp[Array[V1], V2, O]
  = new BinaryUpdateOp[Array[V1], V2, O] {
    def opType = op.opType;
    def apply(a : Array[V1], b : V2) = {
      var i = 0;
      while (i < a.length) {
        a(i) = op(a(i),b);
        i += 1;
      }
    }
  }
  
  /** Set array with scalar. */
  implicit def OpSetArrayScalar[V](implicit s : Scalar[V])
  : CanSet[Array[V],V]
  = new CanSet[Array[V],V] {
    def apply(a : Array[V], b : V) = { 
      var i = 0;
      while (i < a.length) {
        a(i) = b;
        i += 1;
      }
    }
  }
}

object BinaryUpdateOp extends BinaryUpdateOpImplicitsLevel1 {
  
  //
  // Tuples
  //
  
  implicit def OpRecurseTuple2Tuple2[VA1,VA2,VB1,VB2,O<:OpType]
  (implicit op1 : BinaryUpdateOp[VA1,VB1,O], op2 : BinaryUpdateOp[VA2,VB2,O],
   c1 : CompatibleShape[VA1,VB1], c2 : CompatibleShape[VA2,VB2])
  : BinaryUpdateOp[(VA1,VA2), (VB1,VB2), O]
  = new BinaryUpdateOp[(VA1,VA2), (VB1,VB2), O] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2), b : (VB1,VB2)) = {
      op1(a._1,b._1);
      op2(a._2,b._2);
    }
  }
  
  implicit def OpRecurseTuple2Scalar[VA1,VA2,VB,O<:OpType]
  (implicit op1 : BinaryUpdateOp[VA1,VB,O], op2 : BinaryUpdateOp[VA2,VB,O],
   s : Scalar[VB])
  : BinaryUpdateOp[(VA1,VA2), VB, O]
  = new BinaryUpdateOp[(VA1,VA2), VB, O] {
    def opType = op1.opType;
    def apply(a : (VA1,VA2), b : VB) = {
      op1(a._1,b);
      op2(a._2,b);
    }
  }
  
  //
  // Seqs
  //
  
  /** Recurse on elements within a generic seq. */
  implicit def OpRecurseSeqSeq[V1,V2,O<:OpType]
  (implicit op : BinaryUpdateOp[V1,V2,O], c : CompatibleShape[V1,V2])
  : BinaryUpdateOp[scala.collection.Seq[V1], scala.collection.Seq[V2], O]
  = new BinaryUpdateOp[scala.collection.Seq[V1], scala.collection.Seq[V2], O] {
    def opType = op.opType;
    def apply(a : scala.collection.Seq[V1], b : scala.collection.Seq[V2]) = {
      require(a.length == b.length, "Inputs must be the same length");
      for ((av,bv) <- (a.iterator zip b.iterator)) {
        op(av,bv);
      }
    }
  }
  
  /** Recurse on elements within a generic seq. */
  implicit def OpRecurseSeqScalar[V1,V2,O<:OpType]
  (implicit op : BinaryUpdateOp[V1,V2,O], s : Scalar[V2])
  : BinaryUpdateOp[scala.collection.Seq[V1], V2, O]
  = new BinaryUpdateOp[scala.collection.Seq[V1], V2, O] {
    def opType = op.opType;
    def apply(a : scala.collection.Seq[V1], b : V2) = {
      for (v <- a) {
        op(v, b);
      }
    }
  }
  
  
  //
  // Maps
  //
  
  /** Recurse on elements within a generic map. */
  implicit def OpRecurseMapMap[K,V1,V2,O<:OpType]
  (implicit op : BinaryUpdateOp[V1,V2,O], c : CompatibleShape[V1,V2])
  : BinaryUpdateOp[scala.collection.Map[K,V1], scala.collection.Map[K,V2], O]
  = new BinaryUpdateOp[scala.collection.Map[K,V1], scala.collection.Map[K,V2], O] {
    def opType = op.opType;
    def apply(a : scala.collection.Map[K,V1], b : scala.collection.Map[K,V2]) = {
      for (k <- a.keySet) {
        op(a(k),b(k));
      }
      for (k <- b.keySet) {
        if (!a.keySet.contains(k)) {
          op(a(k),b(k));
        }
      }
    }
  }
  
  /** Recurse on elements within a generic map. */
  implicit def OpRecurseMapScalar[K,V1,V2,O<:OpType]
  (implicit op : BinaryUpdateOp[V1,V2,O], s : Scalar[V2])
  : BinaryUpdateOp[scala.collection.Map[K,V1], V2, O]
  = new BinaryUpdateOp[scala.collection.Map[K,V1], V2, O] {
    def opType = op.opType;
    def apply(a : scala.collection.Map[K,V1], b : V2) = {
      for (v <- a.values) {
        op(v,b);
      }
    }
  }
  
  
  //
  // Arrays
  //
  
  /** Recurse on elements within an array. */
  implicit def OpRecurseArrayArray[V1,V2,O<:OpType]
  (implicit op : BinaryUpdateOp[V1,V2,O], c : CompatibleShape[V1,V2])
  : BinaryUpdateOp[Array[V1], Array[V2], O]
  = new BinaryUpdateOp[Array[V1], Array[V2], O] {
    def opType = op.opType;
    def apply(a : Array[V1], b : Array[V2]) = {
      require(a.length == b.length, "Inputs must be the same length");
      var i = 0;
      while (i < a.length) {
        op(a(i),b(i));
        i += 1;
      }
    }
  }
  
  implicit def OpRecurseArrayScalar[V1,V2,O<:OpType]
  (implicit op : BinaryUpdateOp[V1,V2,O], s : Scalar[V2])
  : BinaryUpdateOp[Array[V1], V2, O]
  = new BinaryUpdateOp[Array[V1], V2, O] {
    def opType = op.opType;
    def apply(a : Array[V1], b : V2) = {
      var i = 0;
      while (i < a.length) {
        op(a(i),b);
        i += 1;
      }
    }
  }
}

