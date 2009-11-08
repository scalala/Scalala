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
package scalala.tensor;

import scalala.collection.{DomainException, PartialMap, MutablePartialMap}
import scalala.collection.{MergeableSet, IntSpanSet, ProductSet};

import scalala.tensor.operators._;
import TensorShapes._;

class Tensor2Linearizer[I,J,T<: Tensor2[I,J] with TensorSelfOp[(I,J),T,Shape2], Bound<:Tensor2[I,J]](implicit ops2: TensorArith[(I,J),T,Bound,Shape2])  {
  class ProjectedTensor(private[Tensor2Linearizer] val t: T) extends Tensor1[(I,J)] with TensorSelfOp[(I,J),ProjectedTensor,Shape1Col] {
    def like = new ProjectedTensor(t.like);
    def update(k: (I,J), v: Double) = t.update(k,v);
    def activeDomain = t.activeDomain;
    def domain = t.domain;
    def apply(k: (I,J)) = t(k);
  }
  implicit val ops = new Tensor1Arith[(I,J),ProjectedTensor,Tensor1[(I,J)],Shape1Col];
  implicit val rowOps = new Tensor1Arith[(I,J),ProjectedTensor,Tensor1[(I,J)],Shape1Row];

  def linearize(t: T) = new ProjectedTensor(t);
  def reshape(pt: ProjectedTensor) = pt.t
}
