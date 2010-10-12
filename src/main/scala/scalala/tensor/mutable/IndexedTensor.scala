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

import scalala.generic.{CanAdd,CanSub,CanMul,CanDiv,CanPow,CanMod};
import scalala.generic.{CanAssignInto,CanAddInto,CanSubInto,CanMulInto,CanDivInto,CanPowInto,CanModInto};

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

  //
  // Tensor-scalar
  //

  implicit def canAssignScalarInto[V](scalar : Scalar[V])
  : CanAssignInto[Bound[V],V] = new CanAssignInto[Bound[V],V] {
    override def apply(a : Bound[V], s : V) = {
      if (s == scalar.zero) {
        a.transformNonZeroValues(v => s);
      } else {
        a.transformValues(v => s);
      }
    }
  }

  implicit def canAddScalarInto[V,O](implicit op : CanAdd[V,O,V], so : Scalar[O])
  : CanAddInto[Bound[V],O] = new CanAddInto[Bound[V],O] {
    override def apply(a : Bound[V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }

  implicit def canSubScalarInto[V,O](implicit op : CanSub[V,O,V], so : Scalar[O])
  : CanSubInto[Bound[V],O] = new CanSubInto[Bound[V],O] {
    override def apply(a : Bound[V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }

  implicit def canMulScalarInto[V,O](implicit op : CanMul[V,O,V], so : Scalar[O])
  : CanMulInto[Bound[V],O] = new CanMulInto[Bound[V],O] {
    override def apply(a : Bound[V], b : O) = {
      if (so.isNaN(b)) {
        a.transformValues(v => op(v, b));
      } else {
        a.transformNonZeroValues(v => op(v, b));
      }
    }
  }

  /** Divides each element by the given scale factor. */
  implicit def canDivScalarInto[V,O](implicit op : CanDiv[V,O,V], so : Scalar[O])
  : CanDivInto[Bound[V],O] = new CanDivInto[Bound[V],O] {
    override def apply(a : Bound[V], b : O) = {
      if (b == so.zero || so.isNaN(b)) {
        a.transformValues(v => op(v, b));
      } else {
        a.transformNonZeroValues(v => op(v, b));
      }
    }
  }

  /** Divides each element by the given scale factor. */
  implicit def canPowScalarInto[V,O](implicit op : CanPow[V,O,V], so : Scalar[O])
  : CanDivInto[Bound[V],O] = new CanDivInto[Bound[V],O] {
    override def apply(a : Bound[V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }

  /** Divides each element by the given scale factor. */
  implicit def canModScalarInto[V,O](implicit op : CanMod[V,O,V], so : Scalar[O])
  : CanModInto[Bound[V],O] = new CanModInto[Bound[V],O] {
    override def apply(a : Bound[V], b : O) = {
      a.transformValues(v => op(v, b));
    }
  }


  //
  // Updates from another Tensor.
  //

  implicit def canAssignInto[V1]
  : CanAssignInto[Bound[V1],Tensor[K,V1]] = new CanAssignInto[Bound[V1],Tensor[K,V1]] {
    override def apply(a : Bound[V1], b : Tensor[K,V1]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => b(k));
    }
  }

  implicit def canAssignInto[V1,V2](implicit s : Scalar[V2], tf : (V2=>V1))
  : CanAssignInto[Bound[V1],Tensor[K,V2]] = new CanAssignInto[Bound[V1],Tensor[K,V2]] {
    override def apply(a : Bound[V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => b(k));
    }
  }

  implicit def canAddBoundInto[V1,V2](implicit op : CanAdd[V1,V2,V1], s : Scalar[V2])
  : CanAddInto[Bound[V1],Tensor[K,V2]] = new CanAddInto[Bound[V1],Tensor[K,V2]] {
    override def apply(a : Bound[V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canSubBoundInto[V1,V2](implicit op : CanSub[V1,V2,V1], s : Scalar[V2])
  : CanSubInto[Bound[V1],Tensor[K,V2]] = new CanSubInto[Bound[V1],Tensor[K,V2]] {
    override def apply(a : Bound[V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canMulBoundInto[V1,V2](implicit op : CanMul[V1,V2,V1], s : Scalar[V2])
  : CanMulInto[Bound[V1],Tensor[K,V2]] = new CanMulInto[Bound[V1],Tensor[K,V2]] {
    override def apply(a : Bound[V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canDivBoundInto[V1,V2](implicit op : CanDiv[V1,V2,V1], s : Scalar[V2])
  : CanDivInto[Bound[V1],Tensor[K,V2]] = new CanDivInto[Bound[V1],Tensor[K,V2]] {
    override def apply(a : Bound[V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canModBoundInto[V1,V2](implicit op : CanMod[V1,V2,V1], s : Scalar[V2])
  : CanModInto[Bound[V1],Tensor[K,V2]] = new CanModInto[Bound[V1],Tensor[K,V2]] {
    override def apply(a : Bound[V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }

  implicit def canPowBoundInto[V1,V2](implicit op : CanPow[V1,V2,V1], s : Scalar[V2])
  : CanPowInto[Bound[V1],Tensor[K,V2]] = new CanPowInto[Bound[V1],Tensor[K,V2]] {
    override def apply(a : Bound[V1], b : Tensor[K,V2]) = {
      a.checkDomain(b.domain);
      a.transform((k,v) => op(v,b(k)));
    }
  }
}
