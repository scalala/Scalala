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

import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

/**
 * Marker for being able to filter the values in a value collection.
 *
 * @author dramage
 */
trait CanFilterValues[From, @specialized A, +To] {
  /** Maps all values from the given collection. */
  def filter(from : From, fn : (A=>Boolean)) : To;

  /** Maps all non-zero values from the given collection. */
  def filterNonZero(from : From, fn : (A=>Boolean)) : To;
}

object CanFilterValues {
  type Op[From,A,To] = CanFilterValues[From,A,To];

  //
  // Arrays
  //

  class OpArray[@specialized A:ClassManifest]
  extends Op[Array[A],A,Array[A]] {
    def filter(from : Array[A], fn : (A=>Boolean)) =
      from.filter(fn);
    def filterNonZero(from : Array[A], fn : (A => Boolean)) =
      this.filter(from, fn);
  }

  implicit def opArray[@specialized A:ClassManifest] =
    new OpArray[A];

  implicit object OpArrayI extends OpArray[Int];
  implicit object OpArrayS extends OpArray[Short];
  implicit object OpArrayL extends OpArray[Long];
  implicit object OpArrayF extends OpArray[Float];
  implicit object OpArrayD extends OpArray[Double];

  //
  // SparseArrays
  //
  class OpSparseArray[@specialized V:ClassManifest:DefaultArrayValue]
  extends Op[SparseArray[V],V,SparseArray[V]] {
    def filter(from : SparseArray[V], fn : (V=>Boolean)) =
      from.filter(fn);

    def filterNonZero(from : SparseArray[V], fn : (V=>Boolean)) = {
      val rv = new SparseArray[V](from.length, from.activeLength);
      from.foreachActivePair((k,v) => if (fn(v)) { rv(k) = v; });
      rv.compact;
      rv;
    }
  }

  implicit def opSparseArray[@specialized V:ClassManifest:DefaultArrayValue] =
    new OpSparseArray[V];

  implicit object OpSparseArrayI extends OpSparseArray[Int];
  implicit object OpSparseArrayS extends OpSparseArray[Short];
  implicit object OpSparseArrayL extends OpSparseArray[Long];
  implicit object OpSparseArrayF extends OpSparseArray[Float];
  implicit object OpSparseArrayD extends OpSparseArray[Double];


  //
  // Scala maps
  //

  implicit def opMap[A, B, That](implicit bf : scala.collection.generic.CanBuildFrom[scala.collection.Map[A,B], (A,B), That]) =
    new OpMap[A,B,That];

  class OpMap[A, B, That] extends Op[scala.collection.Map[A,B],B,scala.collection.Map[A,B]] {
    def filter(from : scala.collection.Map[A,B], fn : (B => Boolean)) =
      from.filter((tup : (A,B)) => fn(tup._2));
    def filterNonZero(from : scala.collection.Map[A,B], fn : (B => Boolean)) =
      this.filter(from, fn);
  }
}

