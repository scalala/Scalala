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

import scalala.tensor._;

/**
 * Transpose of a shaped value.
 *
 * @author dramage
 */
trait CanTranspose[-From,+To] extends (From=>To);

/**
 * Transpose non-mutable tensors.
 * 
 * @author dramage
 */
trait CanTransposeImplicitsLevel0 {
  implicit def canTransposeTensor1Row[K,V]
  : CanTranspose[Tensor1Row[K,V],Tensor1Col[K,V]]
  = new CanTranspose[Tensor1Row[K,V],Tensor1Col[K,V]] {
    override def apply(from : Tensor1Row[K,V]) =
      from.t;
  }

  implicit def canTransposeTensor1Col[K,V]
  : CanTranspose[Tensor1Col[K,V],Tensor1Row[K,V]]
  = new CanTranspose[Tensor1Col[K,V],Tensor1Row[K,V]] {
    override def apply(from : Tensor1Col[K,V]) =
      from.t;
  }
  
  implicit def canTransposeTensor2[K2,K1,V]
  : CanTranspose[Tensor2[K1,K2,V],Tensor2[K2,K1,V]]
  = new CanTranspose[Tensor2[K1,K2,V],Tensor2[K2,K1,V]] {
    override def apply(from : Tensor2[K1,K2,V]) = {
      from.t;
    }
  }
}

trait CanTransposeImplicitsLevel0M extends CanTransposeImplicitsLevel0 {
  implicit def canTransposeMutableTensor1Col[K,V]
  : CanTranspose[mutable.Tensor1Col[K,V],mutable.Tensor1Row[K,V]]
  = new CanTranspose[mutable.Tensor1Col[K,V],mutable.Tensor1Row[K,V]] {
    override def apply(from : mutable.Tensor1Col[K,V]) =
      from.t;
  }
  
  implicit def canTransposeMutableTensor1Row[K,V]
  : CanTranspose[mutable.Tensor1Row[K,V],mutable.Tensor1Col[K,V]]
  = new CanTranspose[mutable.Tensor1Row[K,V],mutable.Tensor1Col[K,V]] {
    override def apply(from : mutable.Tensor1Row[K,V]) =
      from.t;
  }
}

trait CanTransposeImplicitsLevel1 extends CanTransposeImplicitsLevel0M {
  implicit def canTransposeVectorRow[V] : CanTranspose[VectorRow[V],VectorCol[V]]
  = new CanTranspose[VectorRow[V],VectorCol[V]] {
    override def apply(from : VectorRow[V]) =
      from.t;
  }

  implicit def canTransposeVectorCol[V] : CanTranspose[VectorCol[V],VectorRow[V]]
  = new CanTranspose[VectorCol[V],VectorRow[V]] {
    override def apply(from : VectorCol[V]) =
      from.t;
  }
  
  implicit def canTransposeMatrix[V] : CanTranspose[Matrix[V],Matrix[V]] =
  new CanTranspose[Matrix[V],Matrix[V]] {
    override def apply(from : Matrix[V]) =
      from.t;
  }
}

trait CanTransposeImplicitsLevel1M extends CanTransposeImplicitsLevel1 {
  implicit def canTransposeMutableVectorCol[V]
  : CanTranspose[mutable.VectorCol[V],mutable.VectorRow[V]]
  = new CanTranspose[mutable.VectorCol[V],mutable.VectorRow[V]] {
    override def apply(from : mutable.VectorCol[V]) =
      from.t;
  }
  
  implicit def canTransposeMutableVectorRow[V]
  : CanTranspose[mutable.VectorRow[V],mutable.VectorCol[V]]
  = new CanTranspose[mutable.VectorRow[V],mutable.VectorCol[V]] {
    override def apply(from : mutable.VectorRow[V]) =
      from.t;
  }
  
  implicit def canTransposeMutableMatrix[V] : CanTranspose[mutable.Matrix[V],mutable.Matrix[V]] =
  new CanTranspose[mutable.Matrix[V],mutable.Matrix[V]] {
    override def apply(from : mutable.Matrix[V]) =
      from.t;
  }
}

trait CanTransposeImplicitsLevel2 {
  implicit def canTransposeSparseVectorRow[V] : CanTranspose[sparse.SparseVectorRow[V],sparse.SparseVectorCol[V]]
  = new CanTranspose[sparse.SparseVectorRow[V],sparse.SparseVectorCol[V]] {
    override def apply(from : sparse.SparseVectorRow[V]) =
      from.t;
  }
  
  implicit def canTransposeSparseVectorCol[V] : CanTranspose[sparse.SparseVectorCol[V],sparse.SparseVectorRow[V]]
  = new CanTranspose[sparse.SparseVectorCol[V],sparse.SparseVectorRow[V]] {
    override def apply(from : sparse.SparseVectorCol[V]) =
      from.t;
  }
  
  implicit def canTransposeDenseVectorRow[V] : CanTranspose[dense.DenseVectorRow[V],dense.DenseVectorCol[V]]
  = new CanTranspose[dense.DenseVectorRow[V],dense.DenseVectorCol[V]] {
    override def apply(from : dense.DenseVectorRow[V]) =
      from.t;
  }
  
  implicit def canTransposeDenseVectorCol[V] : CanTranspose[dense.DenseVectorCol[V],dense.DenseVectorRow[V]]
  = new CanTranspose[dense.DenseVectorCol[V],dense.DenseVectorRow[V]] {
    override def apply(from : dense.DenseVectorCol[V]) =
      from.t;
  }
}

object CanTranspose extends CanTransposeImplicitsLevel2 {
  
}

