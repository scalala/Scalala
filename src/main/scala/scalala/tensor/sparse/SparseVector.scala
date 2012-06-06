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
package sparse;

import domain.{IterableDomain,IndexDomain};


import scalala.collection.sparse.{SparseArray,DefaultArrayValue};
import scalala.scalar.Scalar;

import scalala.operators._
import scalala.generic.collection.{CanCopy, CanCreateZerosLike, CanSliceCol, CanAppendColumns}
import dense.DenseVector
;

/**
 * A vector backed by a SparseArray.
 *
 * @author dramage
 */
@SerialVersionUID(1)
trait SparseVector[@specialized(Int,Long,Float,Double) V]
extends SparseArrayTensor[Int,V] with SparseArrayTensorLike[Int,V,IndexDomain,SparseVector[V]]
with mutable.Vector[V] with mutable.VectorLike[V,SparseVector[V]] with Serializable {
  override def length = data.length;

  override def apply(key : Int) =
    data(key);

  override def update(key : Int, value : V) =
    data(key) = value;

  override def foreachNonZeroKey[U](fn : (Int=>U)) = {
    data.foreachActiveKey(fn);
    data.activeLength == data.length;
  }

  override def foreachNonZeroValue[U](fn : (V=>U)) = {
    data.foreachActiveValue(fn);
    data.activeLength == data.length;
  }

  override def foreachNonZeroPair[U](fn : ((Int,V)=>U)) = {
    data.foreachActivePair(fn);
    data.activeLength == data.length;
  }

  override def pairsIteratorNonZero:Iterator[(Int,V)] = data.activeIterator
}

object SparseVector {
  /** Creates a sparse vector literal. */
  def apply[V:Scalar:ClassManifest:DefaultArrayValue](values : V*) =
    new SparseVectorCol(SparseArray(values :_*))

  /** Returns a vector of zeros of the given size. */
  def zeros[V:Scalar:ClassManifest:DefaultArrayValue](size : Int) =
    create(size)();

  /** Creates a sparse vector of the given size with given initial values. */
  def create[V:Scalar:ClassManifest:DefaultArrayValue](size : Int)(values : (Int,V)*) =
    new SparseVectorCol(SparseArray.create(size)(values :_*));

  /** Tabulate a vector with the value at each offset given by the function. */
  def tabulate[V:Scalar:ClassManifest:DefaultArrayValue](size : Int)(f : (Int => V)) =
    new SparseVectorCol(SparseArray.tabulate(size)(f));


  /*
   * Implicits for functions
   */

    /** Optimized base class for creating zeros */
  trait CanCreateZerosSparseVector
  [@specialized V, @specialized RV, SV[V]<:SparseVector[V]]
    extends CanCreateZerosLike[SV[V],SV[RV]] {
    def create(length : Int) : SV[RV];
    def apply(v1: SV[V]) = create(v1.length);
  }


  class GenericSparseVectorRowBase[@specialized V:Scalar:Manifest:DefaultArrayValue] {
    def create(length : Int) = new SparseVectorRow(new SparseArray[V](length))
  }

  class GenericSparseVectorColBase[@specialized V:Scalar:Manifest:DefaultArrayValue] {
    def create(length : Int) = new SparseVectorCol(new SparseArray[V](length))
  }

  class GenericSparseVectorBase[@specialized V:Scalar:Manifest:DefaultArrayValue] {
    def create(length : Int) = new SparseVectorCol(new SparseArray[V](length))
  }


  implicit def canCreateZerosSparseVector[@specialized V, @specialized RV:Scalar:Manifest:DefaultArrayValue]
  : CanCreateZerosSparseVector[V, RV, SparseVector] =
    new GenericSparseVectorColBase with CanCreateZerosSparseVector[V, RV, SparseVector];

    /** Optimized base class for mapping dense columns. */
  implicit def canCreateZerosSparseVectorCols[@specialized V, @specialized RV:Scalar:Manifest:DefaultArrayValue]
  : CanCreateZerosSparseVector[V, RV, SparseVectorCol] =
  new GenericSparseVectorColBase with CanCreateZerosSparseVector[V, RV, SparseVectorCol];

  /** Optimized base class for mapping dense rows. */
  implicit def canCreateZerosSparseVectorRows[@specialized V, @specialized RV:Scalar:Manifest:DefaultArrayValue]
  : CanCreateZerosSparseVector[V, RV, SparseVectorRow] =
  new GenericSparseVectorRowBase with CanCreateZerosSparseVector[V, RV, SparseVectorRow];


  /** Optimized base class for mapping sparse columns. */
  implicit def canCopySparseVectorCols[@specialized V:Scalar:Manifest] = new CanCopySparseVectorCol[V];

  /** Optimized base class for mapping sparse rows. */
  implicit def canCopySparseVectorRows[@specialized V:Scalar:Manifest] = new CanCopySparseVectorRow[V];

  /** Optimized base class for mapping sparse . */
  implicit def canCopySparseVector[@specialized V:Scalar:Manifest] = new CanCopySparseVector[V];

  /** Optimized base class for copying sparse */
  class CanCopySparseVectorRow[@specialized V:Scalar:ClassManifest] extends CanCopy[SparseVectorRow[V]] {
    def apply(v1: SparseVectorRow[V]) = {
      new SparseVectorRow[V](v1.data.copy)
    }
  }

  class CanCopySparseVector[@specialized V:Scalar:ClassManifest] extends CanCopy[SparseVector[V]] {
    def apply(v1: SparseVector[V]) = {
      new SparseVectorCol[V](v1.data.copy)
    }
  }

  class CanCopySparseVectorCol[@specialized V:Scalar:ClassManifest] extends CanCopy[SparseVectorCol[V]] {
    def apply(v1: SparseVectorCol[V]) = {
      new SparseVectorCol[V](v1.data.copy)
    }
  }

  /* Implicits for operations */
  implicit object SparseVectorDInnerMulSparseVectorD
  extends BinaryOp[SparseVectorRow[Double],SparseVectorCol[Double],OpMulRowVectorBy,Double] {
    override def opType = OpMulRowVectorBy;
    override def apply(a : SparseVectorRow[Double], b : SparseVectorCol[Double]) = {
      require(a.length == b.length, "Vectors must have same length");
        a dot b
    }
  }

  implicit object SparseVectorDDotSparseVectorD
  extends BinaryOp[SparseVector[Double],SparseVector[Double],OpMulInner,Double] {
    override def opType = OpMulInner;
    override def apply(a : SparseVector[Double], b : SparseVector[Double]) = {
      require(a.length == b.length, "Vectors must have same length");
      val aData = a.data
      val bData = b.data
      val activeA = aData.activeLength
      val activeB = bData.activeLength

      var sum = 0.0
      var ai = 0
      var bi = 0
      while(ai < activeA) {
        while(bi < activeB && bData.indexAt(bi) < aData.indexAt(ai))
          bi += 1
        if(bi < activeB && bData.indexAt(bi) == aData.indexAt(ai))
          sum += bData.valueAt(bi) * aData.valueAt(ai)
        ai += 1
      }
      sum
    }
  }

  implicit object SparseVectorDDotDenseVectorD
  extends BinaryOp[SparseVector[Double],DenseVector[Double],OpMulInner,Double] {
    override def opType = OpMulInner;
    override def apply(a : SparseVector[Double], b : DenseVector[Double]) = {
      require(a.length == b.length, "Vectors must have same length");
      val aData = a.data
      val bData = b.data
      val activeA = aData.activeLength
      val bStep = b.stride

      var sum = 0.0
      var ai = 0
      var bPos = 0
      while(ai < activeA) {
        val aIndex = aData.indexAt(ai)
        bPos = bStep * aIndex + b.offset
        sum += bData(bPos) * aData.valueAt(ai)
        ai += 1
      }
      sum
    }
  }

  implicit object DenseVectorDDotSparseVectorD
  extends BinaryOp[DenseVector[Double],SparseVector[Double],OpMulInner,Double] {
    override def opType = OpMulInner;
    override def apply(a : DenseVector[Double], b : SparseVector[Double]) = {
      require(a.length == b.length, "Vectors must have same length");
        b dot a
    }
  }

  implicit object VectorDAddSparseVectorDInto
  extends BinaryUpdateOp[mutable.Vector[Double],SparseVector[Double],OpAdd] {
    override def opType = OpAdd;
    override def apply(a : mutable.Vector[Double], b : SparseVector[Double]) {
      require(a.length == b.length, "Vectors must have same length");
      val bIndex = b.data.indexArray
      val bData = b.data.rawValueArray
      val activeB = b.data.activeLength

      var bi = 0
      while(bi < activeB) {
        a(bIndex(bi)) += bData(bi)
        bi += 1
      }
    }
  }

  implicit object SparseVectorDMulDoubleInto
  extends BinaryUpdateOp[SparseVector[Double],Double,OpMul] {
    override def opType = OpMul;
    override def apply(a : SparseVector[Double], b : Double) {
      val bData = a.data.rawValueArray
      val activeB = a.data.activeLength

      var bi = 0
      while(bi < activeB) {
        bData(bi) *= b
        bi += 1
      }
    }
  }

  implicit object VectorDSubSparseVectorDInto
  extends BinaryUpdateOp[mutable.Vector[Double],SparseVector[Double],OpSub] {
    override def opType = OpSub;
    override def apply(a : mutable.Vector[Double], b : SparseVector[Double]) {
      require(a.length == b.length, "Vectors must have same length");
      val bIndex = b.data.indexArray
      val bData = b.data.rawValueArray
      val activeB = b.data.activeLength

      var bi = 0
      while(bi < activeB) {
        a(bIndex(bi)) -= bData(bi)
        bi += 1
      }
    }
  }

  implicit def binaryOpFromBinaryUpdateOp[SV<:SparseVector[Double],Other,Op<:OpType](implicit copy: CanCopy[SV], op: BinaryUpdateOp[SV,Other,Op]) = {
    new BinaryOp[SV,Other,Op,SV] {
      override def opType = op.opType;
      override def apply(a : SV, b : Other) = {
        val c = copy(a)
        op(c,b)
        c
      }
    }
  }

  implicit val vspace = scalala.operators.bundles.MutableInnerProductSpace.make[Double,scalala.tensor.sparse.SparseVector[Double]]
}

class SparseVectorRow[@specialized(Int,Long,Float,Double) V]
(override val data : SparseArray[V])
(implicit override val scalar : Scalar[V])
extends SparseVector[V] with mutable.VectorRow[V] with mutable.VectorRowLike[V,SparseVectorRow[V]] {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    implicit val dv = implicitly[Scalar[V2]].defaultArrayValue;
    domain match {
      case that : IndexDomain => new SparseVectorRow(new SparseArray[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
  
  override def t : SparseVectorCol[V] =
    new SparseVectorCol(data)(scalar);
}

/**
 * SparseVectors as a column.
 *
 * @author dramage
 */
class SparseVectorCol[@specialized(Int,Long,Float,Double) V]
(override val data : SparseArray[V])
(implicit override val scalar : Scalar[V])
extends SparseVector[V] with mutable.VectorCol[V] with mutable.VectorColLike[V,SparseVectorCol[V]]  {
  override def newBuilder[K2,V2:Scalar](domain : IterableDomain[K2]) = {
    implicit val mf = implicitly[Scalar[V2]].manifest;
    implicit val dv = implicitly[Scalar[V2]].defaultArrayValue;
    domain match {
      case that : IndexDomain => new SparseVectorCol(new SparseArray[V2](that.size)).asBuilder;
      case _ => super.newBuilder[K2,V2](domain);
    }
  }
  
  override def t : SparseVectorRow[V] =
    new SparseVectorRow(data)(scalar);
}

