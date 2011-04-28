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
import scalala.operators._
import java.util.Arrays
;

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

  /** Starting offset within data array. */
  val offset : Int;

  /** Gap between successive elements in data array. */
  val stride : Int;

  override def apply(key : Int) =
    data(offset + key * stride);

  override def update(key : Int, value : V) =
    data(offset + key * stride) = value;

  override def foreachPair[U](fn : ((Int,V)=>U)) = {
    var i = 0;
    var k = offset;
    while (i < length) {
      fn(i, data(k));
      i += 1;
      k += stride;
    }
  }

  override def foreachValue[U](fn : (V=>U)) = {
    var i = 0;
    var k = offset;
    while (i < length) {
      fn(data(k));
      i += 1;
      k += stride;
    }
  }

  override def transformPairs(fn : (Int,V)=>V) = {
    var i = 0;
    var k = offset;
    while (i < length) {
      data(k) = fn(i,data(k));
      i += 1;
      k += stride;
    }
  }

  override def transformValues(fn : V=>V) = {
    var i = 0;
    var k = offset;
    while (i < length) {
      data(k) = fn(data(k));
      i += 1;
      k += stride;
    }
  }

  /** Returns a view of this vector as a row. */
  override def asRow : DenseVectorRow[V] =
    new DenseVectorRow(this.data, offset, stride, length);

  /** Returns a view of this vector as a column.  */
  override def asCol : DenseVectorCol[V] =
    new DenseVectorCol(this.data, offset, stride, length);
}

object DenseVector extends DenseVectorConstructors {
  //
  // Generic optimized routines
  //

  class GenericDenseVectorRowBase[@specialized V:Scalar:Manifest] {
    def create(length : Int) = new DenseVectorRow(new Array[V](length));
  }

  class GenericDenseVectorColBase[@specialized V:Scalar:Manifest] {
    def create(length : Int) = new DenseVectorCol(new Array[V](length));
  }

  /** Optimized base class for joining two dense tensors. */
  class CanJoinDenseVectors[
   @specialized(Int,Long,Float,Double) V1,
   @specialized(Int,Long,Float,Double) V2,
   DV[V]<:DenseVector[V]]
  extends CanJoin[DV[V1], DV[V2], Int, V1, V2] {
    override def joinAll[RV](a : DV[V1], b : DV[V2], fn : (Int,V1,V2)=>RV) = {
      // System.err.println("DENSE!");
      a.checkDomain(b.domain);
      var i = 0;
      var k1 = a.offset;
      var k2 = b.offset;
      while (i < a.length) {
        fn(i, a.data(k1), b.data(k2));
        i += 1;
        k1 += a.stride;
        k2 += b.stride;
      }
    }

    override def joinBothNonZero[RV](a : DV[V1], b : DV[V2], fn : (Int,V1,V2)=>RV) =
      joinAll(a,b,fn);

    override def joinEitherNonZero[RV](a : DV[V1], b : DV[V2], fn : (Int,V1,V2)=>RV) =
      joinAll(a,b,fn);
  }

  /** Optimized base class for joining dense columns. */
  implicit def canJoinDenseVectorCols[V1, V2]
  : CanJoinDenseVectors[V1, V2, DenseVectorCol] =
  new CanJoinDenseVectors[V1, V2, DenseVectorCol];

  /** Optimized base class for joining dense rows. */
  implicit def canJoinDenseVectorRows[V1, V2]
  : CanJoinDenseVectors[V1, V2, DenseVectorRow] =
  new CanJoinDenseVectors[V1, V2, DenseVectorRow];

  /** Optimized base class for mapping a dense tensor. */
  trait CanMapValuesDenseVector
  [@specialized V, @specialized RV, DV[V]<:DenseVector[V]]
  extends CanMapValues[DV[V],V,RV,DV[RV]] {
    def create(length : Int) : DV[RV];

    override def map(from : DV[V], fn : (V=>RV)) = {
      // System.err.println("DENSE!");
      val rv = create(from.length);
      var i = 0;
      var k = from.offset;
      while (i < from.length) {
        rv.data(i) = fn(from.data(k));
        i += 1;
        k += from.stride;
      }
      rv;
    }

    override def mapNonZero(from : DV[V], fn : (V=>RV)) =
      map(from, fn);
  }

  /** Optimized base class for mapping dense columns. */
  implicit def canMapValuesDenseVectorCols[@specialized V, @specialized RV:Scalar:Manifest]
  : CanMapValuesDenseVector[V, RV, DenseVectorCol] =
  new GenericDenseVectorColBase with CanMapValuesDenseVector[V, RV, DenseVectorCol];

  /** Optimized base class for mapping dense rows. */
  implicit def canMapValuesDenseVectorRows[@specialized V, @specialized RV:Scalar:Manifest]
  : CanMapValuesDenseVector[V, RV, DenseVectorRow] =
  new GenericDenseVectorRowBase with CanMapValuesDenseVector[V, RV, DenseVectorRow];

  /** optimized for just mapping densevectors */
  implicit def canMapValuesDenseVectors[@specialized V, @specialized RV:Scalar:Manifest]
  : CanMapValuesDenseVector[V, RV, DenseVector] =
  new GenericDenseVectorColBase with CanMapValuesDenseVector[V, RV, DenseVector];

  /** Optimized base class for mapping a dense tensor. */
  trait CanMapKeyValuePairsDenseVector
  [@specialized V, @specialized RV, DV[V]<:DenseVector[V]]
  extends CanMapKeyValuePairs[DV[V],Int,V,RV,DV[RV]] {
    def create(length : Int) : DV[RV];

    override def map(from : DV[V], fn : (Int,V)=>RV) = {
      // System.err.println("DENSE!");
      val rv = create(from.length);
      var i = 0;
      var k = from.offset;
      while (i < from.length) {
        rv.data(i) = fn(i, from.data(k));
        i += 1;
        k += from.stride;
      }
      rv;
    }

    override def mapNonZero(from : DV[V], fn : (Int,V)=>RV) =
      map(from, fn);
  }

  /** Optimized base class for mapping dense columns. */
  implicit def canMapKeyValuePairsDenseVectorCols[@specialized V, @specialized RV:Scalar:Manifest]
  : CanMapKeyValuePairsDenseVector[V, RV, DenseVectorCol] =
  new GenericDenseVectorColBase with CanMapKeyValuePairsDenseVector[V, RV, DenseVectorCol];

  /** Optimized base class for mapping dense rows. */
  implicit def canMapKeyValuePairsDenseVectorRows[@specialized V, @specialized RV:Scalar:Manifest]
  : CanMapKeyValuePairsDenseVector[V, RV, DenseVectorRow] =
  new GenericDenseVectorRowBase with CanMapKeyValuePairsDenseVector[V, RV, DenseVectorRow];

    implicit def canMapKeyValuePairsDenseVector[@specialized V, @specialized RV:Scalar:Manifest]
  : CanMapKeyValuePairsDenseVector[V, RV, DenseVector] =
  new GenericDenseVectorColBase with CanMapKeyValuePairsDenseVector[V, RV, DenseVector];

  /** Optimized base class for creating zeros */
  trait CanCreateZerosDenseVector
  [@specialized V, @specialized RV, DV[V]<:DenseVector[V]]
    extends CanCreateZerosLike[DV[V],DV[RV]] {
    def create(length : Int) : DV[RV];
    def apply(v1: DV[V]) = create(v1.length);
  }


  implicit def canCreateZerosDenseVector[@specialized V, @specialized RV:Scalar:Manifest]
  : CanCreateZerosDenseVector[V, RV, DenseVector] =
    new GenericDenseVectorColBase with CanCreateZerosDenseVector[V, RV, DenseVector];

    /** Optimized base class for mapping dense columns. */
  implicit def canCreateZerosDenseVectorCols[@specialized V, @specialized RV:Scalar:Manifest]
  : CanCreateZerosDenseVector[V, RV, DenseVectorCol] =
  new GenericDenseVectorColBase with CanCreateZerosDenseVector[V, RV, DenseVectorCol];

  /** Optimized base class for mapping dense rows. */
  implicit def canCreateZerosDenseVectorRows[@specialized V, @specialized RV:Scalar:Manifest]
  : CanCreateZerosDenseVector[V, RV, DenseVectorRow] =
  new GenericDenseVectorRowBase with CanCreateZerosDenseVector[V, RV, DenseVectorRow];


  /** Optimized base class for copying zeros */
  class CanCopyDenseVectorRow[@specialized V:Scalar:ClassManifest] extends CanCopy[DenseVectorRow[V]] {
    def apply(v1: DenseVectorRow[V]) = {
      new DenseVectorRow(Array.tabulate(v1.length)(i => v1(i)));
    }
  }

  class CanCopyDenseVector[@specialized V:Scalar:ClassManifest] extends CanCopy[DenseVector[V]] {
    def apply(v1: DenseVector[V]) = {
      new DenseVectorCol(Array.tabulate(v1.length)(i => v1(i)));
    }
  }

  class CanCopyDenseVectorCol[@specialized V:Scalar:ClassManifest] extends CanCopy[DenseVectorCol[V]] {
    def apply(v1: DenseVectorCol[V]) = {
      new DenseVectorCol(Array.tabulate(v1.length)(i => v1(i)));
    }
  }

    /** Optimized base class for mapping dense columns. */
  implicit def canCopyDenseVectorCols[@specialized V:Scalar:Manifest] = new CanCopyDenseVectorCol[V];

  /** Optimized base class for mapping dense rows. */
  implicit def canCopyDenseVectorRows[@specialized V:Scalar:Manifest] = new CanCopyDenseVectorRow[V];





  //
  // Specialized objects for generic routines
  //

  implicit object CanJoinDVCDDVCD extends CanJoinDenseVectors[Double,Double,DenseVectorCol];

  implicit object CanJoinDVRDDVRD extends CanJoinDenseVectors[Double,Double,DenseVectorRow];

  implicit object CanMapValuesDVCDDVCD extends GenericDenseVectorColBase[Double]
  with CanMapValuesDenseVector[Double,Double,DenseVectorCol];

  implicit object CanMapValuesDVRDDVRD extends GenericDenseVectorRowBase[Double]
  with CanMapValuesDenseVector[Double,Double,DenseVectorRow];

  implicit object CanMapKeyValuePairsDVCDDVCD extends GenericDenseVectorColBase[Double]
  with CanMapKeyValuePairsDenseVector[Double,Double,DenseVectorCol];

  implicit object CanMapKeyValuePairsDVRDDVRD extends GenericDenseVectorRowBase[Double]
  with CanMapKeyValuePairsDenseVector[Double,Double,DenseVectorRow];

  //
  // BLAS routines
  //

  /** BLAS-optimized scala multiply. */
  implicit object DenseVectorDMulDInto
  extends BinaryUpdateOp[DenseVector[Double],Double,OpMul] {
    override def opType = OpMul;
    override def apply(a : DenseVector[Double], b : Double) = {
      // System.err.println("BLAS!");
      org.netlib.blas.Dscal.dscal(a.length, b, a.data, a.offset, a.stride);
    }
  }

  /** BLAS-optimized vector addition. */
  implicit object DenseVectorDAddDenseVectorDInto
  extends BinaryUpdateOp[DenseVector[Double],DenseVector[Double],OpAdd] {
    override def opType = OpAdd;
    override def apply(a : DenseVector[Double], b : DenseVector[Double]) = {
      require(a.length == b.length, "Vectors must have same length");
      // System.err.println("BLAS!");
      org.netlib.blas.Daxpy.daxpy(
        a.length, 1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride);
    }
  }

  /** BLAS-optimized vector-vector inner product. */
  implicit object DenseVectorDInnerMulDenseVectorD
  extends BinaryOp[DenseVectorRow[Double],DenseVectorCol[Double],OpMulRowVectorBy,Double] {
    override def opType = OpMulRowVectorBy;
    override def apply(a : DenseVectorRow[Double], b : DenseVectorCol[Double]) = {
      require(a.length == b.length, "Vectors must have same length");
      // System.err.println("BLAS!");
      org.netlib.blas.Ddot.ddot(
        a.length, a.data, a.offset, a.stride, b.data, b.offset, b.stride);
    }
  }
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
(override val data : Array[V], override val offset : Int,
 override val stride : Int, override val length : Int)
(implicit override val scalar : Scalar[V])
extends DenseVector[V] with mutable.VectorRow[V] with mutable.VectorRowLike[V,DenseVectorRow[V]] {
  def this(data : Array[V])(implicit s : Scalar[V]) =
    this(data, 0, 1, data.length)(s);

  def apply(range : Range) : DenseVectorRow[V] = {
    require(range.last < length, "Range out of bounds");
    new DenseVectorRow[V](data,
      offset = offset + stride * range.start,
      stride = stride * range.step,
      length = range.size);
  }


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

  /**
   * Tabulate a vector with the value at each offset given by the function.
   */
  def tabulate[V: Scalar](size: Int)(f: (Int => V)) = {
    implicit val mf = implicitly[Scalar[V]].manifest;
    new DenseVectorCol(Array.tabulate(size)(f));
  }
}

/**
 * DenseVector as a column.
 *
 * @author dramage
 */
class DenseVectorCol[@specialized(Int,Long,Float,Double) V]
(override val data : Array[V], override val offset : Int,
 override val stride : Int, override val length : Int)
(implicit override val scalar : Scalar[V])
extends DenseVector[V] with mutable.VectorCol[V] with mutable.VectorColLike[V,DenseVectorCol[V]]  {
  def this(data : Array[V])(implicit s : Scalar[V]) =
    this(data, 0, 1, data.length)(s);

  def apply(range : Range) : DenseVectorCol[V] = {
    require(range.last < length, "Range out of bounds");
    new DenseVectorCol[V](data,
      offset = offset + stride * range.start,
      stride = stride * range.step,
      length = range.size);
  }

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

object DenseVectorCol extends DenseVectorColConstructors;

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


  /**Concatenates two or more vectors together into a large vector. Assumes column vectors. */
  def vertcat[V:Scalar](vectors: Vector[V]*) = {
    val size = vectors.foldLeft(0)(_ + _.size);
    val result = zeros[V](size);
    var offset = 0;
    for(v <- vectors) {
      result(offset until (offset + v.size)) := v;
      offset += v.size;
    }
    result;
  }

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

