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

/**
 * Builder trait for creating a view of two tensors by joining two underlying
 * tensors across their (identical) domains.
 *
 * @author dramage
 */
trait CanJoinValues[Repr1, -Repr2, V1, V2, RV, +That] {
  /** Joins on all keys in the domain. */
  def joinAll(a : Repr1, b : Repr2, fn : (V1,V2)=>RV) : That;

  /** Joins when both a and b are non-zero. */
  def joinBothNonZero(a : Repr1, b : Repr2, fn : (V1,V2)=>RV) : That;

  /** Joins when either a or b is non-zero. */
  def joinEitherNonZero(a : Repr1, b : Repr2, fn : (V1,V2)=>RV) : That;
}
