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
package mutable;

import scalala.operators.{BinaryUpdateOp,OpType,UnaryOp,OpSet,OpCast};

/**
 * Base companion methods for tensors that have a known key type,
 * such as Vectors (Int) and Matrices (Int,Int).  This class is
 * almost identical to TensorCompanion but Bound only specifies
 * a value type, not a key type.  There might be a cleverer way
 * to do this with type constructors and inheriting from TensorCompanion.
 *
 * @author dramage
 */
trait IndexedTensorCompanion[K,Bound[V]<:Tensor[K,V]] extends tensor.IndexedTensorCompanion[K,Bound] {
//  implicit def binaryUpdateOp[V1,V2,Op<:OpType](implicit c : BinaryUpdateOp[Tensor[K,V1],V2,Op]) =
//    c.asInstanceOf[BinaryUpdateOp[Bound[V1],V2,Op]];
}

