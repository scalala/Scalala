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

import scalala.scalar.Scalar;
import scalala.tensor.domain.DomainException;
import scalala.collection.sparse.{SparseArray,DefaultArrayValue};

/**
 * Mutation delegate for A :-= B.
 *
 * @author dramage
 */
trait CanSubInto[A,-B] extends BinaryUpdateOp[A,B];

object CanSubInto {
  type Op[A,B] = CanSub[A,B,A];
  type UpdateOp[A,B] = CanSubInto[A,B];
  type SparseArraySparseArrayBase[A,B] = SparseArraySparseArrayUpdateEitherNZOp[A,B];

  //
  // Below is copy-and-pasted between companion objects
  //

  implicit def mkIntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoArrayArray[V1,V2];

  class IntoArrayArray[V1,V2](implicit op : Op[V1,V2])
  extends ArrayArrayUpdateOp[V1,V2] with UpdateOp[Array[V1],Array[V2]];

  implicit object IntoArrayArrayII extends IntoArrayArray[Int,Int];
  implicit object IntoArrayArrayDI extends IntoArrayArray[Double,Int];
  implicit object IntoArrayArrayDD extends IntoArrayArray[Double,Double];

  implicit def mkIntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoArrayScalar[V1,B];

  class IntoArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends ArrayScalarUpdateOp[V1,B] with UpdateOp[Array[V1],B];

  implicit object IntoArrayScalarII extends IntoArrayScalar[Int,Int];
  implicit object IntoArrayScalarDI extends IntoArrayScalar[Double,Int];
  implicit object IntoArrayScalarDD extends IntoArrayScalar[Double,Double];

  implicit def mkIntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  = new IntoSparseArraySparseArray;

  class IntoSparseArraySparseArray[V1,V2](implicit op : Op[V1,V2])
  extends SparseArraySparseArrayBase[V1,V2] with UpdateOp[SparseArray[V1],SparseArray[V2]];

  implicit object IntoSparseArraySparseArrayII extends IntoSparseArraySparseArray[Int,Int];
  implicit object IntoSparseArraySparseArrayDD extends IntoSparseArraySparseArray[Double,Double];
  implicit object IntoSparseArraySparseArrayDI extends IntoSparseArraySparseArray[Double,Int];

  implicit def mkIntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  = new IntoSparseArrayScalar[V1,B];

  class IntoSparseArrayScalar[V1,B](implicit op : Op[V1,B], sb : Scalar[B])
  extends SparseArrayScalarUpdateOp[V1,B] with UpdateOp[SparseArray[V1],B];

  implicit object IntoSparseArrayScalarII extends IntoSparseArrayScalar[Int,Int];
  implicit object IntoSparseArrayScalarDI extends IntoSparseArrayScalar[Double,Int];
  implicit object IntoSparseArrayScalarDD extends IntoSparseArrayScalar[Double,Double];
}
