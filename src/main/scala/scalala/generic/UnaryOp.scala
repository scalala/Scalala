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

import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

/**
 * Base trait for Unary operations from A=>B.
 *
 * @author dramage
 */
trait UnaryOp[@specialized(Int,Long,Float,Double) -A, @specialized +B]
extends (A => B);

/**
 * Casts a value as another value.
 *
 * @author dramage
 */
trait CanCast[@specialized(Int,Long,Float,Double) -A, @specialized(Int,Long,Float,Double) +B]
extends UnaryOp[A,B];

object CanCast {
  implicit object opII extends CanCast[Int,Int] {
    override def apply(v : Int) = v;
  }

  implicit object opIL extends CanCast[Int,Long] {
    override def apply(v : Int) = v;
  }

  implicit object opIF extends CanCast[Int,Float] {
    override def apply(v : Int) = v;
  }

  implicit object opID extends CanCast[Int,Double] {
    override def apply(v : Int) = v;
  }

  implicit object opLL extends CanCast[Long,Long] {
    override def apply(v : Long) = v;
  }

  implicit object opLD extends CanCast[Long,Double] {
    override def apply(v : Long) = v;
  }

  implicit object opFF extends CanCast[Float,Float] {
    override def apply(v : Float) = v;
  }

  implicit object opFD extends CanCast[Float,Double] {
    override def apply(v : Float) = v;
  }

  implicit object opDD extends CanCast[Double,Double] {
    override def apply(v : Double) = v;
  }

  implicit def identity[V] : CanCast[V,V] = new CanCast[V,V] {
    override def apply(v : V) = v;
  }
}

/**
 * Create a 0 value of type That from prototype A.
 * 
 * @author dramage
 */
trait CanCreateZerosLike[-A, +That] extends UnaryOp[A,That];

object CanCreateZerosLike {
  type Op[A,That] = CanCreateZerosLike[A,That];

  //
  // Primitives
  //

  implicit object OpI extends Op[Int,Int]
  { def apply(v : Int) = 0; }

  implicit object OpS extends Op[Short,Short]
  { def apply(v : Short) = 0.asInstanceOf[Short]; }

  implicit object OpL extends Op[Long,Long]
  { def apply(v : Long) = 0l; }

  implicit object OpF extends Op[Float,Float]
  { def apply(v : Float) = 0f; }

  implicit object OpD extends Op[Double,Double]
  { def apply(v : Double) = 0.0; }

  //
  // Scala Maps
  //

  implicit def opMap[K,V] : Op[Map[K,V],Map[K,V]] = new Op[Map[K,V],Map[K,V]] {
    def apply(value : Map[K,V]) = Map[K,V]();
  }

  //
  //
  // Below this line is copy-and-paste between ops' companion objects
  //
  //

  //
  // Arrays
  //

  implicit def opArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV])
    = new OpArray[V,RV];

  class OpArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV])
  extends Op[Array[V],Array[RV]] {
    def apply(value : Array[V]) = value.map(op);
  }

  implicit object OpArrayI extends OpArray[Int,Int];
  implicit object OpArrayS extends OpArray[Short,Short];
  implicit object OpArrayL extends OpArray[Long,Long];
  implicit object OpArrayF extends OpArray[Float,Float];
  implicit object OpArrayD extends OpArray[Double,Double];

  //
  // SparseArrays
  //

  implicit def opSparseArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV], dv : DefaultArrayValue[RV])
    = new OpSparseArray[V,RV];

  class OpSparseArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV], dv : DefaultArrayValue[RV])
  extends Op[SparseArray[V],SparseArray[RV]] {
    def apply(value : SparseArray[V]) = value.map(op);
  }

  implicit object OpSparseArrayI extends OpSparseArray[Int,Int];
  implicit object OpSparseArrayS extends OpSparseArray[Short,Short];
  implicit object OpSparseArrayL extends OpSparseArray[Long,Long];
  implicit object OpSparseArrayF extends OpSparseArray[Float,Float];
  implicit object OpSparseArrayD extends OpSparseArray[Double,Double];

  //
  // Tuples
  //

  implicit def opTuple2[A,RA,B,RB](opA : Op[A,RA], opB : Op[B,RB])
  : Op[(A,B),(RA,RB)] = new Op[(A,B),(RA,RB)] {
    def apply(value : (A,B)) = (opA(value._1), opB(value._2));
  }

  implicit def opTuple3[A,RA,B,RB,C,RC](opA : Op[A,RA], opB : Op[B,RB], opC : Op[C,RC])
  : Op[(A,B,C),(RA,RB,RC)] = new Op[(A,B,C),(RA,RB,RC)] {
    def apply(value : (A,B,C)) = (opA(value._1), opB(value._2), opC(value._3));
  }

  implicit def opTuple4[A,RA,B,RB,C,RC,D,RD](opA : Op[A,RA], opB : Op[B,RB], opC : Op[C,RC], opD : Op[D,RD])
  : Op[(A,B,C,D),(RA,RB,RC,RD)] = new Op[(A,B,C,D),(RA,RB,RC,RD)] {
    def apply(value : (A,B,C,D)) = (opA(value._1), opB(value._2), opC(value._3), opD(value._4));
  }
}

/**
 * Create a 1 value of type That from prototype A.
 *
 * @author dramage
 */
trait CanCreateOnesLike[-A, +That] extends UnaryOp[A,That];

object CanCreateOnesLike {
  type Op[A,That] = CanCreateOnesLike[A,That];

  //
  // Primitives
  //

  implicit object OpI extends Op[Int,Int]
  { def apply(v : Int) = 1; }

  implicit object OpS extends Op[Short,Short]
  { def apply(v : Short) = 1.asInstanceOf[Short]; }

  implicit object OpL extends Op[Long,Long]
  { def apply(v : Long) = 1l; }

  implicit object OpF extends Op[Float,Float]
  { def apply(v : Float) = 1f; }

  implicit object OpD extends Op[Double,Double]
  { def apply(v : Double) = 1.0; }

  //
  // Scala Maps
  //

  implicit def opMap[K,V,RV](implicit op : Op[V,RV]) : Op[Map[K,V],Map[K,RV]] = new Op[Map[K,V],Map[K,RV]] {
    def apply(map : Map[K,V]) = map.mapValues(v => op(v));
  }

  //
  //
  // Below this line is copy-and-paste between ops' companion objects
  //
  //

  //
  // Arrays
  //

  implicit def opArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV])
    = new OpArray[V,RV];

  class OpArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV])
  extends Op[Array[V],Array[RV]] {
    def apply(value : Array[V]) = value.map(op);
  }

  implicit object OpArrayI extends OpArray[Int,Int];
  implicit object OpArrayS extends OpArray[Short,Short];
  implicit object OpArrayL extends OpArray[Long,Long];
  implicit object OpArrayF extends OpArray[Float,Float];
  implicit object OpArrayD extends OpArray[Double,Double];

  //
  // SparseArrays
  //

  implicit def opSparseArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV], dv : DefaultArrayValue[RV])
    = new OpSparseArray[V,RV];

  class OpSparseArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV], dv : DefaultArrayValue[RV])
  extends Op[SparseArray[V],SparseArray[RV]] {
    def apply(value : SparseArray[V]) = value.map(op);
  }

  implicit object OpSparseArrayI extends OpSparseArray[Int,Int];
  implicit object OpSparseArrayS extends OpSparseArray[Short,Short];
  implicit object OpSparseArrayL extends OpSparseArray[Long,Long];
  implicit object OpSparseArrayF extends OpSparseArray[Float,Float];
  implicit object OpSparseArrayD extends OpSparseArray[Double,Double];

  //
  // Tuples
  //

  implicit def opTuple2[A,RA,B,RB](opA : Op[A,RA], opB : Op[B,RB])
  : Op[(A,B),(RA,RB)] = new Op[(A,B),(RA,RB)] {
    def apply(value : (A,B)) = (opA(value._1), opB(value._2));
  }

  implicit def opTuple3[A,RA,B,RB,C,RC](opA : Op[A,RA], opB : Op[B,RB], opC : Op[C,RC])
  : Op[(A,B,C),(RA,RB,RC)] = new Op[(A,B,C),(RA,RB,RC)] {
    def apply(value : (A,B,C)) = (opA(value._1), opB(value._2), opC(value._3));
  }

  implicit def opTuple4[A,RA,B,RB,C,RC,D,RD](opA : Op[A,RA], opB : Op[B,RB], opC : Op[C,RC], opD : Op[D,RD])
  : Op[(A,B,C,D),(RA,RB,RC,RD)] = new Op[(A,B,C,D),(RA,RB,RC,RD)] {
    def apply(value : (A,B,C,D)) = (opA(value._1), opB(value._2), opC(value._3), opD(value._4));
  }
}

/**
 * Construction delegate for -A.
 *
 * @author dramgae
 */
trait CanNeg[-A, +That]
extends UnaryOp[A,That];

object CanNeg {
  type Op[A,That] = CanNeg[A,That];

  //
  // Primitives
  //

  implicit object OpI extends Op[Int,Int]
  { def apply(v : Int) = -v; }

  implicit object OpS extends Op[Short,Short]
  { def apply(v : Short) = (-v).asInstanceOf[Short]; }

  implicit object OpL extends Op[Long,Long]
  { def apply(v : Long) = -v; }

  implicit object OpF extends Op[Float,Float]
  { def apply(v : Float) = -v; }

  implicit object OpD extends Op[Double,Double]
  { def apply(v : Double) = -v; }

  //
  // Scala Maps
  //

  implicit def opMap[K,V,RV](implicit op : Op[V,RV]) : Op[Map[K,V],Map[K,RV]] = new Op[Map[K,V],Map[K,RV]] {
    def apply(map : Map[K,V]) = map.mapValues(v => op(v));
  }

  //
  //
  // Below this line is copy-and-paste between ops' companion objects
  //
  //

  //
  // Arrays
  //

  implicit def opArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV])
    = new OpArray[V,RV];

  class OpArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV])
  extends Op[Array[V],Array[RV]] {
    def apply(value : Array[V]) = value.map(op);
  }

  implicit object OpArrayI extends OpArray[Int,Int];
  implicit object OpArrayS extends OpArray[Short,Short];
  implicit object OpArrayL extends OpArray[Long,Long];
  implicit object OpArrayF extends OpArray[Float,Float];
  implicit object OpArrayD extends OpArray[Double,Double];

  //
  // SparseArrays
  //

  implicit def opSparseArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV], dv : DefaultArrayValue[RV])
    = new OpSparseArray[V,RV];

  class OpSparseArray[V,RV](implicit m : ClassManifest[RV], op : Op[V,RV], dv : DefaultArrayValue[RV])
  extends Op[SparseArray[V],SparseArray[RV]] {
    def apply(value : SparseArray[V]) = value.map(op);
  }

  implicit object OpSparseArrayI extends OpSparseArray[Int,Int];
  implicit object OpSparseArrayS extends OpSparseArray[Short,Short];
  implicit object OpSparseArrayL extends OpSparseArray[Long,Long];
  implicit object OpSparseArrayF extends OpSparseArray[Float,Float];
  implicit object OpSparseArrayD extends OpSparseArray[Double,Double];

  //
  // Tuples
  //

  implicit def opTuple2[A,RA,B,RB](opA : Op[A,RA], opB : Op[B,RB])
  : Op[(A,B),(RA,RB)] = new Op[(A,B),(RA,RB)] {
    def apply(value : (A,B)) = (opA(value._1), opB(value._2));
  }

  implicit def opTuple3[A,RA,B,RB,C,RC](opA : Op[A,RA], opB : Op[B,RB], opC : Op[C,RC])
  : Op[(A,B,C),(RA,RB,RC)] = new Op[(A,B,C),(RA,RB,RC)] {
    def apply(value : (A,B,C)) = (opA(value._1), opB(value._2), opC(value._3));
  }

  implicit def opTuple4[A,RA,B,RB,C,RC,D,RD](opA : Op[A,RA], opB : Op[B,RB], opC : Op[C,RC], opD : Op[D,RD])
  : Op[(A,B,C,D),(RA,RB,RC,RD)] = new Op[(A,B,C,D),(RA,RB,RC,RD)] {
    def apply(value : (A,B,C,D)) = (opA(value._1), opB(value._2), opC(value._3), opD(value._4));
  }
}
