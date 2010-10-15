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

import generic.{CanAssignInto,CanAddInto,CanSubInto,CanMulInto,CanDivInto,CanPowInto,CanModInto};

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
  implicit def canAssignInto[V,O](implicit c : CanAssignInto[Tensor[K,V],O]) =
    c.asInstanceOf[CanAssignInto[Bound[V],O]];

  implicit def canAddInto[V,O](implicit c : CanAddInto[Tensor[K,V],O]) =
    c.asInstanceOf[CanAddInto[Bound[V],O]];

  implicit def canSubInto[V,O](implicit c : CanSubInto[Tensor[K,V],O]) =
    c.asInstanceOf[CanSubInto[Bound[V],O]];

  implicit def canMulInto[V,O](implicit c : CanMulInto[Tensor[K,V],O]) =
    c.asInstanceOf[CanMulInto[Bound[V],O]];

  implicit def canDivInto[V,O](implicit c : CanDivInto[Tensor[K,V],O]) =
    c.asInstanceOf[CanDivInto[Bound[V],O]];

  implicit def canModInto[V,O](implicit c : CanModInto[Tensor[K,V],O]) =
    c.asInstanceOf[CanModInto[Bound[V],O]];

  implicit def canPowInto[V,O](implicit c : CanPowInto[Tensor[K,V],O]) =
    c.asInstanceOf[CanPowInto[Bound[V],O]];
}
