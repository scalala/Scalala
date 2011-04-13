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
package generic;

import domain.CanGetDomain;
import scalala.generic.collection._
import operators.HasValuesMonadic
;

/**
 * For comprehensions on pairs of values from an underlying tensor.  This
 * class can be implicitly viewed as a Map[K,V].
 *
 * @author dramage
 */
trait TensorNonZeroMonadic
[@specialized(Int,Long) K, @specialized(Int,Long,Float,Double) V,
 +This<:Tensor[K,V]] extends HasValuesMonadic[This,V] { self =>

  /** Underlying tensor. */
  def repr : This;

  /** Calls repr.nonzeroSize. */
  def size =
    repr.nonzeroSize;
    
  /** Gets a Monadic for the nonzero pairs. */
  def pairs : TensorNonZeroPairsMonadic[K,V,This] =
    new TensorNonZeroPairsMonadic[K,V,This] { override def repr = self.repr };
    
  /** Gets a Monadic for the nonzero keys. */
  def keys : TensorNonZeroKeysMonadic[K,V,This] =
    new TensorNonZeroKeysMonadic[K,V,This] { override def repr = self.repr };
  
  /** Gets a Monadic for the nonzero values. */
  def values : TensorNonZeroValuesMonadic[K,V,This] =
    new TensorNonZeroValuesMonadic[K,V,This] { override def repr = self.repr };
}

