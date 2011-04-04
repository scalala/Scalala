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
package tensor;
package dense;

import domain.{IterableDomain,IndexDomain};

import scalala.generic.collection._;
import scalala.scalar.Scalar;
import scalala.library.random.MersenneTwisterFast;
import scalala.library.Random;
import scalala.operators._;

/**
 * A vector backed by a dense array.
 *
 * @author dramage
 */
@serializable
@SerialVersionUID(1)
trait DenseVector[@specialized(Int,Long,Float,Double) V]
extends mutable.Vector[V] with mutable.VectorLike[V,DenseVector[V]]
with DenseArrayTensor[Int,V] with DenseArrayTensorLike[Int,V,IndexDomain,DenseVector[V]] { self =>

  override def size = data.length;

  override def domain = IndexDomain(data.length);

  override def apply(key : Int) =
    data(key);

  override def update(key : Int, value : V) =
    data(key) = value;

  override def foreachPair[U](fn : ((Int,V)=>U)) = {
    var i = 0;
    while (i < data.length) {
      fn(i,data(i));
      i += 1;
    }
  }

  override def foreachValue[U](fn : (V=>U)) =
    data.foreach(fn);

  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transformPairs(fn : (Int,V)=>V) = {
    var i = 0;
    while (i < data.length) {
      data(i) = fn(i,data(i));
      i += 1;
    }
  }
  
  /** Tranforms all key value pairs in this map by applying the given function. */
  override def transformValues(fn : V=>V) = {
    var i = 0;
    while (i < data.length) {
      data(i) = fn(data(i));
      i += 1;
    }
  }

  /** Returns a view of this vector as a row. Tightens bound superclass's return value. */
  override def asRow : DenseVectorRow[V] = this match {
    case r : DenseVectorRow[_] => this.asInstanceOf[DenseVectorRow[V]];
    case _ => new DenseVectorRow(this.data);
  }

  /** Returns a view of this vector as a column. Tightens bound superclass's return value.  */
  override def asCol : DenseVectorCol[V] = this match {
    case c : DenseVectorCol[_] => this.asInstanceOf[DenseVectorCol[V]];
    case _ => new DenseVectorCol(this.data);
  }
}

object DenseVector extends DenseVectorConstructors {

//  implicit object DenseVectorCanMapValuesFrom
//  extends DomainMapCanMapValuesFrom[DenseVector,Int,Double,Double,DenseVector] {
//    override def apply(from : DenseVector, fn : (Double=>Double)) = {
//      val data = new Array[Double](from.size);
//      var i = 0;
//      while (i < data.length) {
//        data(i) = fn(from.data(i));
//        i += 1;
//      }
//      new DenseVector(data);
//    }
//
//    override def apply(from : DenseVector, fn : ((Int,Double)=>Double)) = {
//      val data = new Array[Double](from.size);
//      var i = 0;
//      while (i < data.length) {
//        data(i) = fn(i, from.data(i));
//        i += 1;
//      }
//      new DenseVector(data);
//    }
//  }
}

/**
 * Constructors for dense vectors.
 *
 * @author dramage
 */
trait DenseVectorConstructors extends DenseVectorColConstructors;

/**
 * DenseVectors as a row.
 *
 * @author dramage
 */
class DenseVectorRow[@specialized(Int,Long,Float,Double) V]
(override val data : Array[V])
(implicit override val scalar : Scalar[V])
extends DenseVector[V] with mutable.VectorRow[V] with mutable.VectorRowLike[V,DenseVectorRow[V]] {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    domain match {
      case that : IndexDomain => new DenseVectorRow(new Array[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
  
  override def t : DenseVectorCol[V] =
    new DenseVectorCol(data)(scalar);
}

object DenseVectorRow {
  def apply[@specialized(Int,Long,Float,Double) V:Scalar](domain : IndexDomain) = {
    implicit val m = implicitly[Scalar[V]].manifest;
    new DenseVectorRow(new Array[V](domain.size));
  }
}

/**
 * DenseVectors as a column.
 *
 * @author dramage
 */
class DenseVectorCol[@specialized(Int,Long,Float,Double) V]
(override val data : Array[V])
(implicit override val scalar : Scalar[V])
extends DenseVector[V] with mutable.VectorCol[V] with mutable.VectorColLike[V,DenseVectorCol[V]]  {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    domain match {
      case that : IndexDomain => new DenseVectorCol(new Array[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
  
  override def t : DenseVectorRow[V] =
    new DenseVectorRow(data)(scalar);
}

object DenseVectorCol extends DenseVectorColConstructors {
//  /** Tighten bound on super to be a dense in return value. */
//  override implicit def canMulVectorColByRow[V1,V2,RV](implicit mul : BinaryOp[V1,V2,OpMul,RV], scalar : Scalar[RV])
//  = super.canMulVectorColByRow[V1,V2,RV](mul, scalar).asInstanceOf[BinaryOp[DenseVectorCol[V1],tensor.VectorRow[V2],OpMulColVectorBy,DenseMatrix[RV]]];

//  /** Tighten bound on super to be a dense in return value. */
//  override implicit def canAppendMatrixColumns[V]
//  : CanAppendColumns[DenseVectorCol[V],tensor.Matrix[V],DenseMatrix[V]]
//  = super.canAppendMatrixColumns[V].asInstanceOf[CanAppendColumns[DenseVectorCol[V],tensor.Matrix[V], DenseMatrix[V]]];
//
//  /** Tighten bound on super to be a dense in return value. */
//  override implicit def canAppendVectorColumn[V]
//  : CanAppendColumns[DenseVectorCol[V],tensor.VectorCol[V],DenseMatrix[V]]
//  = super.canAppendVectorColumn[V].asInstanceOf[CanAppendColumns[DenseVectorCol[V],tensor.VectorCol[V],DenseMatrix[V]]];
}

trait DenseVectorColConstructors {
  /** Constructs a DenseVector for the given IndexDomain. */
  def apply[S:Scalar](domain : IndexDomain) =
    zeros(domain.size);

  /** Constructs a literal DenseVector. */
  def apply[V:Scalar](values : V*) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseVectorCol(values.toArray);
  }

  /** Dense vector containing the given value for all elements. */
  def fill[V:Scalar](size : Int)(value : V) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseVectorCol(Array.fill(size)(value));
  }

  /** Dense vector of zeros of the given size. */
  def zeros[V](size : Int)(implicit s : Scalar[V]) = {
    if (s.isPrimitive) {
      new DenseVectorCol(s.manifest.newArray(size));
    } else {
      fill(size)(s.zero);
    }
  }

  /** Dense vector of ones of the given size. */
  def ones[V:Scalar](size : Int) =
    fill(size)(implicitly[Scalar[V]].one);

  /** Tabulate a vector with the value at each offset given by the function. */
  def tabulate[V:Scalar](size : Int)(f : (Int => V)) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseVectorCol(Array.tabulate(size)(f));
  }

  /**
   * Returns a vector with numbers from 'from' up to (but not including)
   * 'until' incrementing by 'by' at each step.
   */
  def range(from : Int, until : Int, by : Int = 1) =
    new DenseVectorCol[Int](Array.range(from, until, by));

  /** A vector of the given size with uniform random values between 0 and 1. */
  def rand(size : Int, mt : MersenneTwisterFast = Random.mt) = mt.synchronized {
    tabulate(size)(i => mt.nextDouble);
  }

  /**
   * A vector of the given size with normally distributed random values
   * with mean 0 and standard deviation 1.
   */
  def randn(size : Int, mt : MersenneTwisterFast = Random.mt) = mt.synchronized {
    tabulate(size)(i => mt.nextGaussian);
  }

  /** A vector of the given size of random integers in the range [0..max). */
  def randi(imax : Int, size : Int, mt : MersenneTwisterFast = Random.mt) = mt.synchronized {
    tabulate(size)(i => mt.nextInt(imax));
  }
}

