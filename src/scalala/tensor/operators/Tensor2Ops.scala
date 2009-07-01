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
package scalala.tensor.operators;

import scalala.collection.{MergeableSet, IntSpanSet, ProductSet, DomainException};
import scalala.tensor.{Tensor, Tensor1, Tensor2, Vector, Matrix};

import TensorShapes._;

/** Type aliases for Tensor2 support. */
object Tensor2Types {
  type Tensor2Op[I,J,Bound<:Tensor2[I,J],Value<:Bound] =
    TensorOp[(I,J),Bound,Value,Shape2[I,J]];
}

import Tensor2Types._;
import Tensor1Types._;

/** Implicits for working with Tensor2 instances. */
trait Tensor2Ops {
}

/** Singleton instsance of Tensor2Ops trait. */
object Tensor2Ops extends Tensor2Ops;

/** Operators on Tensor2 instances. */
class RichTensor2Op[I,J,Bound2[X,Y]<:Tensor2[X,Y],Value2[X,Y]<:Bound2[X,Y],MV<:Value2[I,J],Bound1[X]<:Tensor1[X]]
(base : Tensor2Op[I,J,Bound2[I,J],MV])
extends RichTensorOp[(I,J),Bound2[I,J],MV,Shape2[I,J]](base) {
  
  if (!base.domain.isInstanceOf[ProductSet[_,_]]) {
    throw new IllegalArgumentException("Tensor2 instance must have its domain as a ProductSet");
  }
  
  def domain2 = base.domain.asInstanceOf[ProductSet[I,J]];
  
  def t = Tensor2Transpose[I,J,Bound2[I,J],MV,Bound2[J,I],Value2[J,I]](base);
  
  /** Matrix-matrix multiplication */
  def *[K,V2<:Bound2[J,K]] (op : Tensor2Op[J,K,Bound2[J,K],V2]) =
    Tensor2MultTensor2[I,J,K,Bound2,Value2[I,K],MV,V2](base,op);
  
  /** Matrix-tensor multiplication */
  def *[V<:Bound1[J]] (op : ColTensor1Op[J,Bound1[J],Bound1[J]]) =
    Tensor2MultColTensor1[I,J,Bound1,Bound1,Bound2,MV,Bound1[I]](base, op);
}

/** Type-safe transposes of a Tensor2. */
case class Tensor2Transpose[I,J,Bound<:Tensor2[I,J],Value<:Bound,BoundTranspose<:Tensor2[J,I],ValueTranspose<:BoundTranspose]
(op : Tensor2Op[I,J,Bound,Value])
extends Tensor2Op[J,I,BoundTranspose,ValueTranspose] {
  override def domain = op.domain.asInstanceOf[ProductSet[I,J]].transpose;
  override def value = op.value.transpose.asInstanceOf[ValueTranspose];
  override def working = op.working.asInstanceOf[ValueTranspose];
  override def create[J](d : MergeableSet[J]) = op.create(d);
}

/** Multiplication of tensor2 to tensor2. */
case class Tensor2MultTensor2[I,INNER,J,Bound2[X,Y]<:Tensor2[X,Y],Value<:Bound2[I,J],V1<:Bound2[I,INNER],V2<:Bound2[INNER,J]]
(a : Tensor2Op[I,INNER,Bound2[I,INNER],V1], b : Tensor2Op[INNER,J,Bound2[INNER,J],V2])
extends Tensor2Op[I,J,Bound2[I,J],Value] {
  if (a.domain.asInstanceOf[ProductSet[I,INNER]]._2 != b.domain.asInstanceOf[ProductSet[INNER,J]]._1)
    throw new DomainException;
  
  override def domain =
    ProductSet[I,J](a.domain.asInstanceOf[ProductSet[I,INNER]]._1, b.domain.asInstanceOf[ProductSet[INNER,J]]._2);
  
  override def value = {
    val innerDomain = a.domain.asInstanceOf[ProductSet[I,INNER]]._2;
    val av = a.value;
    val bv = b.value;
    val rv = create(domain).asInstanceOf[Value];
    for (i <- domain._1; j <- domain._2) {
      rv(i,j) = av.getRow(i) dot bv.getCol(j);
    }
    rv;
  }
  
  override def create[J](d : MergeableSet[J]) = a.create(d);
}

/** Right multiplication of a Tensor2 by a column. */
case class Tensor2MultColTensor1[I,J,Bound1[X]<:Tensor1[X],VV[X]<:Bound1[X],Bound2[X,Y]<:Tensor2[X,Y],MV<:Bound2[I,J],Value<:VV[I]]
(a : Tensor2Op[I,J,Bound2[I,J],MV], b : ColTensor1Op[J,Bound1[J],VV[J]])
extends RowTensor1Op[I,Bound1[I],Value] {
  if (a.domain.asInstanceOf[ProductSet[I,J]]._2 != b.domain)
    throw new DomainException;
    
  override def domain = a.domain.asInstanceOf[ProductSet[I,J]]._1;
  
  override def value = {
    val mv = a.value;
    val vv = b.value;
    val rv = create(domain).asInstanceOf[Value];
    for (i <- domain) {
      rv(i) = mv.getRow(i) dot vv;
    }
    rv;
  }
  override def create[J](d : MergeableSet[J]) = b.create(d);
}
