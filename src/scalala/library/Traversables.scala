/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalala.library;

import scala.collection.TraversableLike;
import scala.collection.generic.CanBuildFrom;

/**
 * Basic math on arbitrary Traversables.
 *
 * @author dramage
 */
trait Traversables {

  def abs[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.abs(i));

  def abs[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.abs(convert(i)));

  def log[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.log(i));

  def log[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.log(convert(i)));

  def exp[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.exp(i));

  def exp[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.exp(convert(i)));

  def pow[This <: Traversable[Double], That](v : TraversableLike[Double,This], exponent : Double)
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.pow(i, exponent));

  def pow[V, This <: Traversable[V], That](v : TraversableLike[V,This], exponent : Double)
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.pow(convert(i), exponent));

  def sqrt[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.sqrt(i));

  def sqrt[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.sqrt(convert(i)));

  def sin[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.sin(i));

  def sin[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.sin(convert(i)));

  def cos[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.cos(i));

  def cos[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.cos(convert(i)));

  def tan[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.tan(i));

  def tan[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.tan(convert(i)));

  def asin[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.asin(i));

  def asin[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.asin(convert(i)));

  def acos[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.acos(i));

  def acos[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.acos(convert(i)));

  def atan[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.atan(i));

  def atan[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.atan(convert(i)));

  def ceil[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.ceil(i));

  def ceil[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.ceil(convert(i)));

  def floor[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.floor(i));

  def floor[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.floor(convert(i)));

  def round[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Long, That]) : That =
    v.map(i => Math.round(i));

  def round[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Long, That], convert : V=>Double) : That =
    v.map(i => Math.round(convert(i)));

  def signum[This <: Traversable[Double], That](v : TraversableLike[Double,This])
  (implicit bf: CanBuildFrom[This, Double, That]) : That =
    v.map(i => Math.signum(i));

  def signum[V, This <: Traversable[V], That](v : TraversableLike[V,This])
  (implicit bf: CanBuildFrom[This, Double, That], convert : V=>Double) : That =
    v.map(i => Math.signum(convert(i)));
}

object Traversables extends Traversables {
}

