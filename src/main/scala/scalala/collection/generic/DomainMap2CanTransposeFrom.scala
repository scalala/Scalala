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
package collection;
package generic;

import domain._;

/**
 * Builder trait for creating a transpose of the given input DomainMap2 This
 * to an output type That.
 *
 * @author dramage
 */
trait DomainMap2CanTransposeFrom
[-From,
 @specialized(Int,Long) A1,
 @specialized(Int,Long) A2,
 @specialized(Int,Long,Float,Double,Boolean) B,
 D1<:IterableDomain[A1] with DomainLike[A1,D1],
 D2<:IterableDomain[A2] with DomainLike[A2,D2],
 D<:Product2DomainLike[A1,A2,D1,D2,T,D],
 T<:Product2DomainLike[A2,A1,D2,D1,D,T],
 +To] {
  def apply(in : From) : To;
}
