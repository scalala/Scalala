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
package collection;

import scalala.scalar.Complex
import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

/**
 * Marker for being able to map the values in a value collection.
 *
 * @author dramage
 */
trait CanMapValues[From, @specialized A, @specialized B, +To] {
  /** Maps all values from the given collection. */
  def map(from : From, fn : (A=>B)) : To;

  /** Maps all non-zero values from the given collection. */
  def mapNonZero(from : From, fn : (A=>B)) : To;
}

object CanMapValues {
  type Op[From,A,B,To] = CanMapValues[From,A,B,To];

  //
  // Arrays
  //

  class OpArray[@specialized A, @specialized B:ClassManifest]
  extends Op[Array[A],A,B,Array[B]] {
    def map(from : Array[A], fn : (A=>B)) =
      from.map(fn);
    def mapNonZero(from : Array[A], fn : (A => B)) =
      this.map(from, fn);
  }

  implicit def opArray[@specialized A, @specialized B:ClassManifest] =
    new OpArray[A,B];

  implicit object OpArrayII extends OpArray[Int,Int];
  implicit object OpArraySS extends OpArray[Short,Short];
  implicit object OpArrayLL extends OpArray[Long,Long];
  implicit object OpArrayFF extends OpArray[Float,Float];
  implicit object OpArrayDD extends OpArray[Double,Double];
  implicit object OpArrayCC extends OpArray[Complex,Complex];
  implicit object OpArrayID extends OpArray[Int,Double];
  implicit object OpArraySD extends OpArray[Short,Double];
  implicit object OpArrayLD extends OpArray[Long,Double];
  implicit object OpArrayFD extends OpArray[Float,Double];

  //
  // SparseArrays
  //

  class OpSparseArray[@specialized V, @specialized RV:ClassManifest:DefaultArrayValue]
  extends Op[SparseArray[V],V,RV,SparseArray[RV]] {
    def map(from : SparseArray[V], fn : (V=>RV)) =
      from.map(fn);

    def mapNonZero(from : SparseArray[V], fn : (V => RV)) = {
      val rv = new SparseArray[RV](from.length, from.activeLength);
      from.foreachActivePair((k,v) => rv(k) = fn(v));
      rv;
    }
  }

  implicit def opSparseArray[@specialized V, @specialized RV:ClassManifest:DefaultArrayValue] =
    new OpSparseArray[V,RV];

  implicit object OpSparseArrayII extends OpSparseArray[Int,Int];
  implicit object OpSparseArraySS extends OpSparseArray[Short,Short];
  implicit object OpSparseArrayLL extends OpSparseArray[Long,Long];
  implicit object OpSparseArrayFF extends OpSparseArray[Float,Float];
  implicit object OpSparseArrayDD extends OpSparseArray[Double,Double];
  implicit object OpSparseArrayCC extends OpSparseArray[Complex,Complex];
  implicit object OpSparseArrayID extends OpSparseArray[Int,Double];
  implicit object OpSparseArraySD extends OpSparseArray[Short,Double];
  implicit object OpSparseArrayLD extends OpSparseArray[Long,Double];
  implicit object OpSparseArrayFD extends OpSparseArray[Float,Double];

  //
  // Scala maps
  //

  implicit def opMap[A, B, O, That](implicit bf : scala.collection.generic.CanBuildFrom[scala.collection.Map[A,B], (A,O), That]) =
    new OpMap[A,B,O,That];

  class OpMap[K, A, B, That](implicit bf : scala.collection.generic.CanBuildFrom[scala.collection.Map[K,A], (K,B), That])
  extends Op[scala.collection.Map[K,A],A,B,That] {
    def map(from : scala.collection.Map[K,A], fn : (A => B)) =
      from.map(tup => (tup._1, fn(tup._2)));
    def mapNonZero(from : scala.collection.Map[K,A], fn : (A => B)) =
      this.map(from, fn);
  }
}
