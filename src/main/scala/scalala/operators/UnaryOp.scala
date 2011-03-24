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
trait UnaryOp[@specialized -This, O<:OpType, +That]
extends (This => That);

object UnaryOp {
  //
  // Casting
  //

  implicit object OpCastII extends UnaryOp[Int,OpCast,Int]
    { override def apply(v : Int) = v; }

  implicit object OpCastIL extends UnaryOp[Int,OpCast,Long]
    { override def apply(v : Int) = v; }

  implicit object OpCastIF extends UnaryOp[Int,OpCast,Float]
    { override def apply(v : Int) = v; }

  implicit object OpCastID extends UnaryOp[Int,OpCast,Double]
    { override def apply(v : Int) = v; }

  implicit object OpCastLL extends UnaryOp[Long,OpCast,Long]
    { override def apply(v : Long) = v; }

  implicit object OpCastLD extends UnaryOp[Long,OpCast,Double]
    { override def apply(v : Long) = v; }

  implicit object OpCastFF extends UnaryOp[Float,OpCast,Float]
    { override def apply(v : Float) = v; }

  implicit object OpCastFD extends UnaryOp[Float,OpCast,Double]
    { override def apply(v : Float) = v; }

  implicit object OpCastDD extends UnaryOp[Double,OpCast,Double]
    { override def apply(v : Double) = v; }

  implicit def OpCastIdentity[V] : UnaryOp[V,OpCast,V] = new UnaryOp[V,OpCast,V]
    { override def apply(v : V) = v; }
  
  //
  // Negation
  //
  
  implicit object OpNegI extends UnaryOp[Int,OpNeg,Int]
    { def apply(v : Int) = -v; }

  implicit object OpNegS extends UnaryOp[Short,OpNeg,Short]
    { def apply(v : Short) = (-v).asInstanceOf[Short]; }

  implicit object OpNegL extends UnaryOp[Long,OpNeg,Long]
    { def apply(v : Long) = -v; }

  implicit object OpNegF extends UnaryOp[Float,OpNeg,Float]
    { def apply(v : Float) = -v; }

  implicit object OpNegD extends UnaryOp[Double,OpNeg,Double]
    { def apply(v : Double) = -v; }
    
    
  //
  // Tuples
  //

  implicit def OpTuple2[V1,V2,RV1,RV2,O<:OpType]
  (implicit op1 : UnaryOp[V1,O,RV1], op2 : UnaryOp[V2,O,RV2])
  : UnaryOp[(V1,V2),O,(RV1,RV2)]
  = new UnaryOp[(V1,V2),O,(RV1,RV2)] {
    def apply(v : (V1,V2)) = (op1(v._1), op2(v._2));
  }

  implicit def OpTuple3[V1,V2,V3,RV1,RV2,RV3,O<:OpType]
  (implicit op1 : UnaryOp[V1,O,RV1], op2 : UnaryOp[V2,O,RV2],
   op3 : UnaryOp[V3,O,RV3])
  : UnaryOp[(V1,V2,V3),O,(RV1,RV2,RV3)]
  = new UnaryOp[(V1,V2,V3),O,(RV1,RV2,RV3)] {
    def apply(v : (V1,V2,V3)) = (op1(v._1), op2(v._2), op3(v._3));
  }
  
  //
  // Maps
  //
  
  implicit def OpMap[K,V,RV,O<:OpType,M,That]
  (implicit view : M=>scala.collection.Map[K,V],
    op : UnaryOp[V,O,RV], bf : CanBuildFrom[M,(K,RV),That])
  : UnaryOp[M, O, That]
  = new UnaryOp[M, O, That] {
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

