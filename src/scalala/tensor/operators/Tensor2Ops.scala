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

/** Type aliases for Tensor2 support. */
object Tensor2Types {
  type Tensor2Op[I,J,Bound<:Tensor2[I,J],Value<:Bound] =
    TensorOp[(I,J),Bound,Value,(I,J)];
}

import Tensor2Types._;
import Tensor1Types._;

/** Implicits for working with Tensor2 instances. */
trait Tensor2Ops {
  protected val sharedTensor2OpBuilderImpl =
    new Tensor2OpBuilderImpl[Any,Any,Tensor2,Tensor1]();
  
  implicit def iTensor2OpBuilderImpl[I,J] =
    new Tensor2OpBuilderImpl[I,J,Tensor2,Tensor1]();
  
  implicit def iTensor2ToTensor2Op[I,J](x : Tensor2[I,J])
  (implicit builder : Tensor2OpBuilderImpl[I,J,Tensor2,Tensor1]) =
    builder.mkTensorIdentity(x);
  
  implicit def iTensor2OpToRichTensor2Op[I,J]
  (op : Tensor2Op[I,J,Tensor2[I,J],Tensor2[I,J]])
  (implicit builder : Tensor2OpBuilderImpl[I,J,Tensor2,Tensor1]) =
    new RichTensor2Op[I,J,Tensor2,Tensor2,Tensor1](op)(builder);
  
  implicit def iTensor2ToRichTensor2Op[I,J]
  (x : Tensor2[I,J])
  (implicit builder : Tensor2OpBuilderImpl[I,J,Tensor2,Tensor1]) =
    new RichTensor2Op[I,J,Tensor2,Tensor2,Tensor1](x)(builder);
  
//  implicit def iTensor2OpToTensor2[I,J]
//  (op : Tensor2Op[I,J,Tensor2[I,J],Tensor2[I,J]]) =
//    op.value;
}

/** Singleton instsance of Tensor2Ops trait. */
object Tensor2Ops extends Tensor2Ops;

/** Builder for Tensor2 operators. */
trait Tensor2OpBuilder[I,J,Bound2[X,Y]<:Tensor2[X,Y],Bound1[X]<:Tensor1[X]]
extends TensorOpBuilder[(I,J),Bound2[I,J],(I,J)] {
  
  def mkTensor2Transpose[Value[X,Y]<:Bound2[X,Y]]
  (op : TensorOp[(I,J),Bound2[I,J],Value[I,J],(I,J)]) =
    Tensor2Transpose[I,J,Bound2[I,J],Value[I,J],Bound2[J,I],Value[J,I]](op);
  
  def mkTensor2MultTensor2[I,INNER,J,Value<:Bound2[I,J],V1<:Bound2[I,INNER],V2<:Bound2[INNER,J]]
  (a : Tensor2Op[I,INNER,Bound2[I,INNER],V1], b : Tensor2Op[INNER,J,Bound2[INNER,J],V2]) =
    Tensor2MultTensor2[I,INNER,J,Bound2,Value,V1,V2](a,b);
  
  def mkTensor2MultColTensor1[I,J,VV[J]<:Bound1[J],MV<:Bound2[I,J],Value<:VV[I]]
  (a : Tensor2Op[I,J,Bound2[I,J],MV], b : ColTensor1Op[J,Bound1[J],VV[J]]) =
    Tensor2MultColTensor1[I,J,Bound1,VV,Bound2,MV,Value](a,b);
}

class Tensor2OpBuilderImpl[I,J,Bound2[X,Y]<:Tensor2[X,Y],Bound1[X]<:Tensor1[X]]
extends Tensor2OpBuilder[I,J,Bound2,Bound1];

/** Operators on Tensor2 instances. */
class RichTensor2Op[I,J,Bound2[X,Y]<:Tensor2[X,Y],Value2[X,Y]<:Bound2[X,Y],Bound1[X]<:Tensor1[X]]
(base : TensorOp[(I,J),Bound2[I,J],Value2[I,J],(I,J)])
(implicit ops : Tensor2OpBuilder[I,J,Bound2,Bound1])
extends RichTensorOp[(I,J),Bound2[I,J],Value2[I,J],(I,J)](base) {
  
  if (!base.domain.isInstanceOf[ProductSet[_,_]]) {
    throw new IllegalArgumentException("Tensor2 instance must have its domain as a ProductSet");
  }
  
  def domain2 = base.domain.asInstanceOf[ProductSet[I,J]];
  
  def t = ops.mkTensor2Transpose[Value2](base);
  
  /** Matrix-matrix multiplication */
  def *[K,V2<:Bound2[J,K]] (op : Tensor2Op[J,K,Bound2[J,K],V2]) =
    ops.mkTensor2MultTensor2[I,J,K,Value2[I,K],Value2[I,J],V2](base,op);
  
  /** Matrix-matrix multiplication */
  def *[K,V2<:Bound2[J,K]] (v : V2)(implicit vOps : Tensor2OpBuilder[J,K,Bound2,Bound1]) =
    ops.mkTensor2MultTensor2[I,J,K,Value2[I,K],Value2[I,J],V2](base, vOps.mkTensorIdentity(v));
  
  /** Matrix-tensor multiplication */
  def * (op : ColTensor1Op[J,Bound1[J],Bound1[J]]) =
    ops.mkTensor2MultColTensor1[I,J,Bound1,Value2[I,J],Bound1[I]](base, op);
  
  /** Matrix-tensor multplication */
  def * (v : Bound1[J])(implicit tOps : ColTensor1OpBuilder[J,Bound1[J],Value2[J,J]]) =
   ops.mkTensor2MultColTensor1[I,J,Bound1,Value2[I,J],Bound1[I]](base, tOps.mkTensorIdentity(v));
}

/** Type-safe transposes of a Tensor2. */
case class Tensor2Transpose[I,J,Bound<:Tensor2[I,J],Value<:Bound,BoundTranspose<:Tensor2[J,I],ValueTranspose<:BoundTranspose]
(op : TensorOp[(I,J),Bound,Value,(I,J)])
extends TensorOp[(J,I),BoundTranspose,ValueTranspose,(J,I)] {
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
