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

import scalala.tensor._;
import scalala.tensor.domain._;
import scalala.tensor.dense._;
import scalala.tensor.generic.TensorBuilder;
import scalala.scalar.Scalar;

/**
 * Trait for constructing a lazy view of a given Tensor.
 *
 * @author dramage
 */
trait CanBuildTensorFrom[-From, Domain, K, V, +To] {
  def apply(from : From, domain : Domain) : TensorBuilder[K,V,To];
}

/**
 * Base level implicits take any tensor and any domain.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel0 {
  implicit def canBuildTensorFromTensor[K,V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], IterableDomain[K], K, V, mutable.Tensor[K,V]]
  = new CanBuildTensorFrom[Tensor[_,_], IterableDomain[K], K, V, mutable.Tensor[K,V]] {
    override def apply(from : Tensor[_,_], domain : IterableDomain[K]) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[K,V,mutable.Tensor[K,V]]];
  }
}

/**
 * Implicits that take any shaped tensor (Tensor1Row, Tensor1Col, Tensor2)
 * and a shaped domain and return a similarly shaped tensor.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel1 extends CanBuildTensorFromImplicitsLevel0 {
  implicit def canBuildTensor1RowFromTensor1Row[K,V:Scalar]
  : CanBuildTensorFrom[Tensor1Row[_,_], Domain1[K], K, V, mutable.Tensor1Row[K,V]]
  = new CanBuildTensorFrom[Tensor1Row[_,_], Domain1[K], K, V, mutable.Tensor1Row[K,V]] {
    override def apply(from : Tensor1Row[_,_], domain : Domain1[K]) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[K,V,mutable.Tensor1Row[K,V]]];
  }
  
  implicit def canBuildTensor1ColFromTensor1Row[K,V:Scalar]
  : CanBuildTensorFrom[Tensor1Col[_,_], Domain1[K], K, V, mutable.Tensor1Col[K,V]]
  = new CanBuildTensorFrom[Tensor1Col[_,_], Domain1[K], K, V, mutable.Tensor1Col[K,V]] {
    override def apply(from : Tensor1Col[_,_], domain : Domain1[K]) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[K,V,mutable.Tensor1Col[K,V]]];
  }

  implicit def canBuildTensor2FromTensor[K1,K2,V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], Domain2[K1,K2], (K1,K2), V, mutable.Tensor2[K1,K2,V]]
  = new CanBuildTensorFrom[Tensor[_,_], Domain2[K1,K2], (K1,K2), V, mutable.Tensor2[K1,K2,V]] {
    override def apply(from : Tensor[_,_], domain : Domain2[K1,K2]) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[(K1,K2),V,mutable.Tensor2[K1,K2,V]]];
  }
}

/**
 * Implicits that take any tensor and a vector/matrix domain and return a
 * vector or matrix.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel2 extends CanBuildTensorFromImplicitsLevel1 {
  implicit def canBuildVectorColFromTensor[V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], IndexDomain, Int, V, mutable.VectorCol[V]]
  = new CanBuildTensorFrom[Tensor[_,_], IndexDomain, Int, V, mutable.VectorCol[V]] {
    override def apply(from : Tensor[_,_], domain : IndexDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[Int,V,mutable.VectorCol[V]]];
  }
  
  implicit def canBuildMatrixFromTensor[V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], TableDomain, (Int,Int), V, mutable.Matrix[V]]
  = new CanBuildTensorFrom[Tensor[_,_], TableDomain, (Int,Int), V, mutable.Matrix[V]] {
    override def apply(from : Tensor[_,_], domain : TableDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[(Int,Int),V,mutable.Matrix[V]]];
  }
}

/**
 * Keep row shape in Level 2.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel2Row extends CanBuildTensorFromImplicitsLevel2 {
  implicit def canBuildVectorRowFromTensor1Row[V:Scalar]
  : CanBuildTensorFrom[Tensor1Row[_,_], IndexDomain, Int, V, mutable.VectorRow[V]]
  = new CanBuildTensorFrom[Tensor1Row[_,_], IndexDomain, Int, V, mutable.VectorRow[V]] {
    override def apply(from : Tensor1Row[_,_], domain : IndexDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[Int,V,mutable.VectorRow[V]]];
  }
}

/**
 * Implicits that keep the type and shape of common input vectors and matrices.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel3 extends CanBuildTensorFromImplicitsLevel2Row {
  import dense._;
  import sparse._;
  import mutable.Counter;
  
  implicit def canBuildCounterFromCounter[K,V:Scalar]
  : CanBuildTensorFrom[Counter[_,_], Domain1[K], K, V, Counter[K,V]]
  = new CanBuildTensorFrom[Counter[_,_], Domain1[K], K, V, Counter[K,V]] {
    override def apply(from : Counter[_,_], domain : Domain1[K]) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[K,V,Counter[K,V]]];
  }
  
  implicit def canBuildDenseVectorColFromDenseTensor[V:Scalar]
  : CanBuildTensorFrom[DenseArrayTensor[_,_], IndexDomain, Int, V, DenseVectorCol[V]]
  = new CanBuildTensorFrom[DenseArrayTensor[_,_], IndexDomain, Int, V, DenseVectorCol[V]] {
    override def apply(from : DenseArrayTensor[_,_], domain : IndexDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[Int,V,DenseVectorCol[V]]];
  }
  
  implicit def canBuildDenseMatrixFromDenseTensor[V:Scalar]
  : CanBuildTensorFrom[DenseArrayTensor[_,_], TableDomain, (Int,Int), V, DenseMatrix[V]]
  = new CanBuildTensorFrom[DenseArrayTensor[_,_], TableDomain, (Int,Int), V, DenseMatrix[V]] {
    override def apply(from : DenseArrayTensor[_,_], domain : TableDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[(Int,Int),V,DenseMatrix[V]]];
  }
  
  implicit def canBuildSparseVectorColFromSparseTensor[V:Scalar]
  : CanBuildTensorFrom[SparseArrayTensor[_,_], IndexDomain, Int, V, SparseVectorCol[V]]
  = new CanBuildTensorFrom[SparseArrayTensor[_,_], IndexDomain, Int, V, SparseVectorCol[V]] {
    override def apply(from : SparseArrayTensor[_,_], domain : IndexDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[Int,V,SparseVectorCol[V]]];
  }
}

/**
 * Keep row shape in Level 3.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel3Row extends CanBuildTensorFromImplicitsLevel3 {
  import dense._;
  import sparse._;

  implicit def canBuildDenseVectorRowFromDenseVectorRow[V:Scalar]
  : CanBuildTensorFrom[DenseVectorRow[_], IndexDomain, Int, V, DenseVectorRow[V]]
  = new CanBuildTensorFrom[DenseVectorRow[_], IndexDomain, Int, V, DenseVectorRow[V]] {
    override def apply(from : DenseVectorRow[_], domain : IndexDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[Int,V,DenseVectorRow[V]]];
  }

  implicit def canBuildSparseVectorRowFromSparseVectorRow[V:Scalar]
  : CanBuildTensorFrom[SparseVectorRow[_], IndexDomain, Int, V, SparseVectorRow[V]]
  = new CanBuildTensorFrom[SparseVectorRow[_], IndexDomain, Int, V, SparseVectorRow[V]] {
    override def apply(from : SparseVectorRow[_], domain : IndexDomain) =
      from.newBuilder(domain).asInstanceOf[TensorBuilder[Int,V,SparseVectorRow[V]]];
  }
}

object CanBuildTensorFrom extends CanBuildTensorFromImplicitsLevel3Row;

