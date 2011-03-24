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
import scalala.tensor.mutable.TensorBuilder;
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
 * Base level implicits take any tensor.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel0 {
  implicit def canBuildTensorFromTensor[K,V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], IterableDomain[K], K, V, mutable.Tensor[K,V]]
  = new CanBuildTensorFrom[Tensor[_,_], IterableDomain[K], K, V, mutable.Tensor[K,V]] {
    override def apply(from : Tensor[_,_], domain : IterableDomain[K]) = {
      domain match {
        case d : IndexDomain => DenseVectorCol(d).asBuilder;
        case d : TableDomain => DenseMatrix(d).asBuilder;
        case _ => mutable.Tensor[K,V](domain).asBuilder;
      }
    }
  }
}

/**
 * Implicits that take any shaped tensor (Tensor1Row, Tensor1Col, Tensor2)
 * and return a similarly shaped tensor.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel1 extends CanBuildTensorFromImplicitsLevel0 {
  implicit def canBuildTensor1RowFromTensor1Row[K,V:Scalar]
  : CanBuildTensorFrom[Tensor1Row[_,_], IterableDomain[K], K, V, mutable.Tensor1Row[K,V]]
  = new CanBuildTensorFrom[Tensor1Row[_,_], IterableDomain[K], K, V, mutable.Tensor1Row[K,V]] {
    override def apply(from : Tensor1Row[_,_], domain : IterableDomain[K]) = {
      domain match {
        case d : IndexDomain => DenseVectorRow[V](d).asBuilder;
        case _ => mutable.Tensor1Row[K,V](domain).asBuilder;
      }
    }
  }
  
  implicit def canBuildTensor1ColFromTensor1Row[K,V:Scalar]
  : CanBuildTensorFrom[Tensor1Col[_,_], IterableDomain[K], K, V, mutable.Tensor1Col[K,V]]
  = new CanBuildTensorFrom[Tensor1Col[_,_], IterableDomain[K], K, V, mutable.Tensor1Col[K,V]] {
    override def apply(from : Tensor1Col[_,_], domain : IterableDomain[K]) = {
      domain match {
        case d : IndexDomain => DenseVectorCol[V](d).asBuilder;
        case _ => mutable.Tensor1Col[K,V](domain).asBuilder;
      }
    }
  }

  implicit def canBuildTensor2FromTensor[K1,K2,V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], Product2Domain[K1,K2], (K1,K2), V, mutable.Tensor2[K1,K2,V]]
  = new CanBuildTensorFrom[Tensor[_,_], Product2Domain[K1,K2], (K1,K2), V, mutable.Tensor2[K1,K2,V]] {
    override def apply(from : Tensor[_,_], domain : Product2Domain[K1,K2]) = {
      domain match {
        case d : TableDomain => DenseMatrix[V](d).asBuilder;
        case _ => mutable.Tensor2[K1,K2,V](domain).asBuilder;
      }
    }
  }
}

/**
 * Implicits that make a default dense (column) vector from an IndexDomain
 * or matrix from a table domain.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel2 extends CanBuildTensorFromImplicitsLevel1 {
  implicit def canBuildVectorFromTensor[V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], IndexDomain, Int, V, DenseVectorCol[V]]
  = new CanBuildTensorFrom[Tensor[_,_], IndexDomain, Int, V, DenseVectorCol[V]] {
    override def apply(from : Tensor[_,_], domain : IndexDomain) =
      DenseVectorCol[V](domain).asBuilder;
  }
  
  implicit def canBuildMatrixFromTensor[V:Scalar]
  : CanBuildTensorFrom[Tensor[_,_], TableDomain, (Int,Int), V, DenseMatrix[V]]
  = new CanBuildTensorFrom[Tensor[_,_], TableDomain, (Int,Int), V, DenseMatrix[V]] {
    override def apply(from : Tensor[_,_], domain : TableDomain) =
      DenseMatrix[V](domain).asBuilder;
  }
}

/**
 * Implicits that make a default dense (row) vector from an IndexDomain.
 *
 * @author dramage
 */
trait CanBuildTensorFromImplicitsLevel3 extends CanBuildTensorFromImplicitsLevel2 {
  implicit def canBuildVectorRowFromTensorRow[V:Scalar]
  : CanBuildTensorFrom[Tensor1Row[_,_], IndexDomain, Int, V, DenseVectorRow[V]]
  = new CanBuildTensorFrom[Tensor1Row[_,_], IndexDomain, Int, V, DenseVectorRow[V]] {
    override def apply(from : Tensor1Row[_,_], domain : IndexDomain) =
      DenseVectorRow[V](domain).asBuilder;
  }
}

object CanBuildTensorFrom extends CanBuildTensorFromImplicitsLevel3;

