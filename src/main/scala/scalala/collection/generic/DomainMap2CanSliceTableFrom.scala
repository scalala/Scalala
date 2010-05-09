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
 * Builder trait for a DomainMap2SliceTable.
 *
 * @author dramage
 */
trait DomainMap2CanSliceTableFrom
[-From, @specialized A1, @specialized A2, @specialized B,
 D1<:IterableDomain[A1], D2<:IterableDomain[A2],
 D<:Product2Domain[A1,A2,D1,D2], +To] {
  def apply(from : From, keys1 : Seq[A1], keys2 : Seq[A2]) : To;
}
