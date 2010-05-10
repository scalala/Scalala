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
package scalala;
package tensor;

import collection.{DomainException, PartialMap, MutablePartialMap}
import collection.{MergeableSet, IntSpanSet, ProductSet};

import tensor.operators._;
import TensorShapes._;

trait TensorLinearizer[I,J,T<:Tensor[I], ProjectedTensor <: Tensor1[J] with TensorSelfOp[J,ProjectedTensor,Shape1Col]] {
  implicit val ops = new Tensor1Arith[J,ProjectedTensor,Tensor1[J],Shape1Col];
  implicit val rowOps = new Tensor1Arith[J,ProjectedTensor,Tensor1[J],Shape1Row];
  def linearize(t: T):ProjectedTensor;
  def reshape(pt: ProjectedTensor):T;
}

object TensorLinearizer  {
  class DefaultProjectedTensor[I,T<:Tensor[I] with TensorSelfOp[I,T,_]](private[TensorLinearizer] val t: T)
      extends Tensor1[I] with TensorSelfOp[I,DefaultProjectedTensor[I,T],Shape1Col] {
      def like = new DefaultProjectedTensor[I,T](t.like);
      def update(k: I, v: Double) = t.update(k,v);
      def activeDomain = t.activeDomain;
      def domain = t.domain;
      def apply(k: I) = t(k);
    }

  implicit def apply[I,T<:Tensor[I] with TensorSelfOp[I,T,_]]() = new DefaultTensorLinearizer[I,T]();

  class DefaultTensorLinearizer[I,T<:Tensor[I] with TensorSelfOp[I,T,_]] 
        extends TensorLinearizer[I, I, T, DefaultProjectedTensor[I,T]]  {
    type ProjectedTensor = DefaultProjectedTensor[I,T];
    def linearize(t: T) = new ProjectedTensor(t);
    def reshape(pt: ProjectedTensor) = pt.t
  }

}