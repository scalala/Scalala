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
import scala.collection.generic.CanBuildFrom;

/**
 * Operation of type O that returns That from arguments A and B.
 *
 * @author dramage
 */
@implicitNotFound(msg="Could not find a way to ${O} value of type ${This}")
trait UnaryOp[@specialized -This, O<:OpType, +That] {
  // should not inherit from This=>That because the compiler will try to use it as a conversion.
  def opType : O;
  def apply(t: This):That
}

object UnaryOp extends UnaryTupleOps {
  
  //
  // Negation
  //
  
  implicit object OpNegI extends UnaryOp[Int,OpNeg,Int]
    { def opType = OpNeg; def apply(v : Int) = -v; }

  implicit object OpNegS extends UnaryOp[Short,OpNeg,Short]
    { def opType = OpNeg; def apply(v : Short) = (-v).asInstanceOf[Short]; }

  implicit object OpNegL extends UnaryOp[Long,OpNeg,Long]
    { def opType = OpNeg; def apply(v : Long) = -v; }

  implicit object OpNegF extends UnaryOp[Float,OpNeg,Float]
    { def opType = OpNeg; def apply(v : Float) = -v; }

  implicit object OpNegD extends UnaryOp[Double,OpNeg,Double]
    { def opType = OpNeg; def apply(v : Double) = -v; }
  
  //
  // Not
  //
  
  implicit object OpNotB extends UnaryOp[Boolean,OpNot,Boolean]
    { def opType = OpNot; def apply(v : Boolean) = !v; }
  
  //
  // Maps
  //
  
  implicit def OpMap[K,V,RV,O<:OpType,M,That]
  (implicit view : M=>scala.collection.Map[K,V],
    op : UnaryOp[V,O,RV], bf : CanBuildFrom[M,(K,RV),That])
  : UnaryOp[M, O, That]
  = new UnaryOp[M, O, That] {
    def opType = op.opType;
    def apply(m : M) = {
      val builder = bf(m);
      for ((k,v) <- m) {
        builder += k -> op(v);
      }
      builder.result;
    }
  }
  
  //
  // Seqs
  //

  implicit def OpSeq[V,RV,O<:OpType,S,That]
  (implicit view : S=>scala.collection.Seq[V], op : UnaryOp[V,O,RV],
   bf : CanBuildFrom[S,RV,That])
  : UnaryOp[S,O,That]
  = new UnaryOp[S,O,That] {
    def opType = op.opType;
    def apply(s : S) = {
      val builder = bf(s);
      for (v <- s) {
        builder += op(v);
      }
      builder.result;
    }
  }
  
  //
  // Arrays
  //

  implicit def OpArray[V,RV,O<:OpType]
  (implicit op : UnaryOp[V,O,RV], mf : Manifest[RV])
  : UnaryOp[Array[V],O,Array[RV]]
  = new UnaryOp[Array[V],O,Array[RV]] {
    def opType = op.opType;
    def apply(a : Array[V]) = {
      var rv = new Array[RV](a.length);
      var i = 0;
      while (i < rv.length) {
        rv(i) = op(a(i));
        i += 1;
      }
      rv;
    }
  }
}

