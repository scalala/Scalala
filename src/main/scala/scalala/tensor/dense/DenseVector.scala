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
import bundles.MutableInnerProductSpace
import java.util.Arrays

/**
 * A vector backed by a dense array.
 *
 * @author dramage
 */
@SerialVersionUID(1)
trait DenseVector[@specialized(Int,Long,Float,Double) V]
extends mutable.Vector[V] with mutable.VectorLike[V,DenseVector[V]]
with DenseArrayTensor[Int,V] with DenseArrayTensorLike[Int,V,IndexDomain,DenseVector[V]]
{ self =>

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

trait LowPriorityDenseImplicits {
  class CanJoinDenseWithNonDense[
    @specialized(Int,Long,Float,Double) V1,
    @specialized(Int,Long,Float,Double) V2,
    DV[V]<:DenseVector[V]] extends CanJoin[DV[V1], Vector[V2], Int, V1, V2] {
    def joinAll[RV](a: DV[V1], b: Vector[V2], fn: (Int, V1, V2) => RV) {
      a.checkDomain(b.domain)
      for( (i,v2) <- b.pairs) {
        fn(i,a(i),v2)
      }

    }

    def joinBothNonZero[RV](a: DV[V1], b: Vector[V2], fn: (Int, V1, V2) => RV) {
      joinAll(a,b,fn)
    }

    def joinEitherNonZero[RV](a: DV[V1], b: Vector[V2], fn: (Int, V1, V2) => RV) {
      joinAll(a,b,fn)
    }
  }

  /** Optimized base class for joining dense columns. */
    implicit def canJoinDenseVectorColsWithNonDense[V1, V2]
  : CanJoinDenseWithNonDense[V1, V2, DenseVectorCol] =
  new CanJoinDenseWithNonDense[V1, V2, DenseVectorCol];

  /** Optimized base class for joining dense rows. */
  implicit def canJoinDenseVectorRowsWithNonDense[V1, V2]
  : CanJoinDenseWithNonDense[V1, V2, DenseVectorRow] =
  new CanJoinDenseWithNonDense[V1, V2, DenseVectorRow];


}

object DenseVector extends DenseVectorConstructors with LowPriorityDenseImplicits {
  //
  // Generic optimized routines
  //

  class GenericDenseVectorRowBase[@specialized V:Scalar:Manifest] {
    def create(length : Int) = new DenseVectorRow(new Array[V](length));
  }

  class GenericDenseVectorColBase[@specialized V:Scalar:Manifest] {
    def create(length : Int) = new DenseVectorCol(new Array[V](length));
  }

  class GenericDenseVectorBase[@specialized V:Scalar:Manifest] {
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
  trait CanZipMapValuesDenseVector
  [@specialized V, @specialized RV, DV[V]<:DenseVector[V]]
    extends CanZipMapValues[DV[V],V,RV,DV[RV]] {
    def create(length : Int) : DV[RV];

    /**Maps all corresponding values from the two collection. */
    def map(from: DV[V], from2: DV[V], fn: (V, V) => RV) = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }
  }

  /** Optimized base class for mapping dense columns. */
  implicit def canZipMapValuesDenseVectorCols[@specialized V, @specialized RV:Scalar:Manifest]
  : CanZipMapValuesDenseVector[V, RV, DenseVectorCol] =
    new GenericDenseVectorColBase with CanZipMapValuesDenseVector[V, RV, DenseVectorCol];

  /** Optimized base class for mapping dense rows. */
  implicit def canZipMapValuesDenseVectorRows[@specialized V, @specialized RV:Scalar:Manifest]
  : CanZipMapValuesDenseVector[V, RV, DenseVectorRow] =
    new GenericDenseVectorRowBase with CanZipMapValuesDenseVector[V, RV, DenseVectorRow];

  /** optimized for just mapping densevectors */
  implicit def canZipMapValuesDenseVectors[@specialized V, @specialized RV:Scalar:Manifest]
  : CanZipMapValuesDenseVector[V, RV, DenseVector] =
    new GenericDenseVectorColBase with CanZipMapValuesDenseVector[V, RV, DenseVector];



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

  implicit object CanMapValuesDV extends GenericDenseVectorBase[Double]
  with CanMapValuesDenseVector[Double,Double,DenseVector];

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

    /** BLAS-optimized vector subtraction. */
  implicit object DenseVectorDSubDenseVectorDInto
  extends BinaryUpdateOp[DenseVector[Double],DenseVector[Double],OpSub] {
    override def opType = OpSub;
    override def apply(a : DenseVector[Double], b : DenseVector[Double]) = {
      require(a.length == b.length, "Vectors must have same length");
      // System.err.println("BLAS!");
      org.netlib.blas.Daxpy.daxpy(
        a.length, -1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride);
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


  /** BLAS-optimized forwarding calls from UpdateOp to Op */
  implicit def binaryOpFromBinaryUpdateOp[DV<:DenseVector[Double],Other,Op<:OpType](implicit copy: CanCopy[DV], op: BinaryUpdateOp[DV,Other,Op]) = {
    new BinaryOp[DV,Other,Op,DV] {
      override def opType = op.opType;
      override def apply(a : DV, b : Other) = {
        val c = copy(a)
        op(c,b)
        c
      }
    }
  }

  implicit def denseVectorColVSpace:MutableInnerProductSpace[Double,DenseVectorCol[Double]] = {
    MutableInnerProductSpace.make[Double,DenseVectorCol[Double]]
  };

  implicit def denseVectorRowVSpace:MutableInnerProductSpace[Double,DenseVectorRow[Double]] = {
    MutableInnerProductSpace.make[Double,DenseVectorRow[Double]]
  };

  implicit def denseVectorVSpace:MutableInnerProductSpace[Double,DenseVector[Double]] = {
    MutableInnerProductSpace.make[Double,DenseVector[Double]]
  };

}

/**
 * DenseVectors as a row.
 *
 * @author dramage
 */
final class DenseVectorRow[@specialized(Int,Long,Float,Double) V]
(override final val data : Array[V], override final val offset : Int,
 override val stride : Int, override val length : Int)
(implicit override val scalar : Scalar[V])
extends DenseVector[V] with mutable.VectorRow[V] with mutable.VectorRowLike[V,DenseVectorRow[V]] {
  def this(data : Array[V])(implicit s : Scalar[V]) =
    this(data, 0, 1, data.length)(s);

  // we have this pointless override to encourage hotspot to inline
  @inline override def apply(key: Int) = data(offset + key * stride);

  def apply(range : Range) : DenseVectorRow[V] = {
    if(range.isEmpty) DenseVectorRow.zeros[V](0)
    else {
      require(range.last < length, "Range out of bounds");
      new DenseVectorRow[V](data,
        offset = offset + stride * range.start,
        stride = stride * range.step,
        length = range.size);
    }
  }

  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    domain match {
      case that : IndexDomain => new DenseVectorRow(new Array[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }

  override def t : DenseVectorCol[V] =
    new DenseVectorCol(data, offset, stride, length)(scalar);

  // Override asRow to avoid unnecessary copies.
  @inline override def asRow: DenseVectorRow[V] = this
}

/**
 * DenseVector as a column.
 *
 * @author dramage
 */
final class DenseVectorCol[@specialized(Int,Long,Float,Double) V]
(override val data : Array[V], override val offset : Int,
 override val stride : Int, override val length : Int)
(implicit override val scalar : Scalar[V])
extends DenseVector[V] with mutable.VectorCol[V] with mutable.VectorColLike[V,DenseVectorCol[V]] with Serializable {
  def this(data : Array[V])(implicit s : Scalar[V]) =
    this(data, 0, 1, data.length)(s);

  // we have this pointless override to encourage hotspot to inline
  @inline override def apply(key: Int) = data(offset + key * stride);

  def apply(range : Range) : DenseVectorCol[V] = {
    if(range.isEmpty) DenseVectorCol.zeros[V](0)
    else {
      require(range.last < length, "Range out of bounds");
      new DenseVectorCol[V](data,
        offset = offset + stride * range.start,
        stride = stride * range.step,
        length = range.size);
    }
  }

  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    domain match {
      case that : IndexDomain => new DenseVectorCol(new Array[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }

  override def t : DenseVectorRow[V] =
    new DenseVectorRow(data, offset, stride, length)(scalar);

  // Override asCol to avoid unnecessary copies.
  @inline override def asCol: DenseVectorCol[V] = this
}

/**
 * Common constructors shared by DenseVectorRow and DenseVectorCol.
 * @author dramage, afwlehmann
 *
 */
trait CommonDenseVectorConstructors [+This[_] <: DenseVector[_]] {
  /**
   * Constructs a DenseVector from the given values.
   */
  def apply[@specialized V: Scalar](values: Array[V]): This[V]

  /**
   * Constructs a DenseVector from the given values.
   */
  def apply[@specialized V: Scalar](values: V*): This[V] = {
    implicit val mf = implicitly[Scalar[V]].manifest
    apply(values.toArray)
  }

  /**
   * Constructs a DenseVector for the given IndexDomain.
   */
  def apply[@specialized V: Scalar](domain: IndexDomain): This[V] =
    zeros(domain.size)

  /**
   * Dense vector containing the given value for all elements.
   */
  def fill[V: Scalar](size: Int)(value: V): This[V] = {
    implicit val mf = implicitly[Scalar[V]].manifest
    // this is too slow: apply(Array.fill(size)(value))
    if(mf.getClass.isAssignableFrom(java.lang.Double.TYPE)) {
      val arr = new Array[V](size)
      Arrays.fill(arr.asInstanceOf[Array[Double]],value.asInstanceOf[Double])
      apply(arr)
    } else if(mf.getClass.isAssignableFrom(java.lang.Float.TYPE)) {
      val arr = new Array[V](size)
      Arrays.fill(arr.asInstanceOf[Array[Float]],value.asInstanceOf[Float])
      apply(arr)
    } else if(mf.getClass.isAssignableFrom(java.lang.Integer.TYPE)) {
      val arr = new Array[V](size)
      Arrays.fill(arr.asInstanceOf[Array[Int]],value.asInstanceOf[Int])
      apply(arr)
    } else if(mf.getClass.isAssignableFrom(java.lang.Long.TYPE)) {
      val arr = new Array[V](size)
      Arrays.fill(arr.asInstanceOf[Array[Long]],value.asInstanceOf[Long])
      apply(arr)
    } else {
      apply(Array.fill(size)(value))
    }
  }

  /**
   * DenseVector of zeros of the given size.
   */
  def zeros[V](size: Int)(implicit s: Scalar[V]): This[V] = {
    if (s.isPrimitive)
      apply(s.manifest.newArray(size))
    else
      fill(size)(s.zero)
  }

  /**
   * DenseVector of ones of the given size.
   */
  def ones[V: Scalar](size: Int): This[V] =
    fill(size)(implicitly[Scalar[V]].one)

  /**
   * Tabulate a DenseVector with the value at each offset given by the function.
   */
  def tabulate[V: Scalar](size: Int)(f: (Int => V)): This[V] = {
    implicit val mf = implicitly[Scalar[V]].manifest
    apply(Array.tabulate(size)(f))
  }

  /**
   * Returns a vector with numbers from 'from' up to (but not including)
   * 'until' incrementing by 'by' at each step.
   */
  def range(from: Int, until: Int, by: Int = 1): This[Int] =
    apply[Int](Array.range(from, until, by))

  /**
   * A vector of the given size with uniform random values from [0,1).
   */
  def rand(size: Int, mt: MersenneTwisterFast = Random.mt): This[Double] =
    mt.synchronized {
      tabulate(size)(i => mt.nextDouble)
    }

  /**
   * A vector of the given size with normally distributed random values
   * with mean 0 and standard deviation 1.
   */
  def randn(size: Int, mt: MersenneTwisterFast = Random.mt): This[Double] =
    mt.synchronized {
      tabulate(size)(i => mt.nextGaussian)
    }

  /**
   * A vector of the given size of random integers in the range [0..max).
   */
  def randi(imax: Int, size: Int, mt: MersenneTwisterFast = Random.mt): This[Int] =
    mt.synchronized {
      tabulate(size)(i => mt.nextInt(imax))
    }
}

/**
 * @author dramage, afwlehmann
 */
object DenseVectorRow extends CommonDenseVectorConstructors[DenseVectorRow] {
  /**
   * {@inheritDoc}
   */
  override def apply[@specialized V: Scalar](values: Array[V]): DenseVectorRow[V] =
    new DenseVectorRow[V](values)

  /**
   * Horizontal concatenation of two or more row vectors into one large vector.
   */
  def horzcat[V: Scalar](vectors: VectorRow[V]*): DenseVectorRow[V] = {
    val size = vectors.foldLeft(0)(_ + _.size)
    val result = zeros[V](size)
    var offset = 0
    for (v <- vectors) {
      result(offset until (offset + v.size)) := v
      offset += v.size
    }
    result
  }

  /**
   * Vertical concatenation of two or more row vectors into one matrix.
   * @throws IllegalArgumentException if vectors have different sizes
   */
  def vertcat[V: Scalar](vectors: VectorRow[V]*): DenseMatrix[V] = {
    val size = vectors.head.size
    if (!(vectors forall (_.size == size)))
      throw new IllegalArgumentException("All vectors must have the same size!")
    val result = DenseMatrix.zeros[V](vectors.size, size)
    for ((v, row) <- vectors zip (0 until vectors.size))
      result(row,::) := v
    result
  }
}

/**
 * @author dramage, afwlehmann
 */
trait DenseVectorColConstructors extends CommonDenseVectorConstructors[DenseVectorCol] {
  /**
   * {@inheritDoc}
   */
  override def apply[@specialized V: Scalar](values: Array[V]): DenseVectorCol[V] =
    new DenseVectorCol[V](values)

  /**
   * Horizontal concatenation of two or more row vectors into one matrix.
   * @throws IllegalArgumentException if vectors have different sizes
   */
  def horzcat[V: Scalar](vectors: VectorCol[V]*): DenseMatrix[V] = {
    val size = vectors.head.size
    if (!(vectors forall (_.size == size)))
      throw new IllegalArgumentException("All vectors must have the same size!")
    val result = DenseMatrix.zeros[V](vectors.size, size)
    for ((v, col) <- vectors zip (0 until vectors.size))
      result(::,col) := v
    result
  }

  /**
   * Vertical concatenation of two or more column vectors into one large vector.
   */
  def vertcat[V: Scalar](vectors: VectorCol[V]*): DenseVectorCol[V] = {
    val size = vectors.foldLeft(0)(_ + _.size)
    val result = zeros[V](size)
    var offset = 0
    for (v <- vectors) {
      result(offset until (offset + v.size)) := v
      offset += v.size
    }
    result
  }
}

object DenseVectorCol extends DenseVectorColConstructors

trait DenseVectorConstructors extends DenseVectorColConstructors

