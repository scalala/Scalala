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
package operators;

import collection.{MergeableSet, IntSpanSet, ProductSet, DomainException};
import TensorShapes._;

/** Type aliases for Tensor2 support. */
object Tensor2Types {
  type Tensor2Op[+Value<:Tensor2[_,_]] =
    TensorOp[Value,Shape2];
}

import Tensor2Types._;
import Tensor1Types._;

trait Tensor2Ops {
  implicit def tensor2Arith[I,J] = new TensorArith[(I,J),Tensor2[I,J],Tensor2[I,J],Shape2];

}

trait CholeskyDecomposer[M<:Tensor2[_,_],R<:Tensor2[_,_]] {
  def decompose(m: Tensor2Op[M]): Tensor2Op[R];
}

trait MatrixTranspose[V<:Tensor2[_,_],VT<:Tensor2[_,_]] {
  def makeTranspose(op: Tensor2Op[V]): Tensor2Op[VT];
}

trait TensorSolver[V<:Tensor2[_,_],V2<:Tensor[_],VR<:Tensor[_],S2<:TensorShape,SR<:TensorShape] {
  def solve(v: TensorOp[V,Shape2], v2: TensorOp[V2,S2]): TensorOp[VR,SR];
}

trait TensorPower[V<:Tensor2[_,_],VR<:Tensor2[_,_]] {
  def power(v: TensorOp[V,Shape2], power: Double): TensorOp[VR,Shape2];
}


/** Singleton instsance of Tensor2Ops trait. */
object Tensor2Ops extends Tensor2Ops;

/** Operators on Tensor2 instances. */
class RichTensor2Op[MV<:Tensor2[_,_]](base : Tensor2Op[MV])
    extends RichTensorOp[MV,Shape2](base) {
  
  def t[VT<:Tensor2[_,_]](implicit ops: MatrixTranspose[MV,VT]) =
    ops.makeTranspose(base);

  def \ [V2<:Tensor[_],S2<:TensorShape,VR<:Tensor[_],SR<:TensorShape](op : TensorOp[V2,S2])
    (implicit solver: TensorSolver[MV,V2,VR,S2,SR]) = {
    solver.solve(base,op);
  }

  def ^[VR<:Tensor2[_,_]](op : Double)(implicit power: TensorPower[MV,VR]) = {
    power.power(base,op);
  }

  
  /** Matrix-matrix multiplication */
  def *[V2<:Tensor[_],VR<:Tensor[_],S2<:TensorShape,SR<:TensorShape] (op : TensorOp[V2,S2])
    (implicit ops: TensorProductBuilder[MV,V2,VR,Shape2,S2,SR]) ={
    ops.makeProduct(base,op);
  }

}

/** Type-safe transposes of a Tensor2. */
case class Tensor2Transpose[I,J,Value<:Tensor2[I,J]]
(op : Tensor2Op[Value])
extends Tensor2Op[Tensor2[J,I]] {
  override def value = op.value.transpose;
  override def working = op.working.transpose;
}

/** Multiplication of tensor2 to tensor2. */
case class Tensor2MultTensor2[I,INNER,J,Value<:Tensor2[I,J],V1<:Tensor2[I,INNER],V2<:Tensor2[INNER,J]]
(a : Tensor2Op[V1], b : Tensor2Op[V2])
(implicit tpB: TensorProductBuilder[V1,V2,Value,Shape2,Shape2,Shape2])
extends Tensor2Op[Value] {
  override def value = {
    val av = a.value;
    val bv = b.value;
    val rv = tpB.create(av,bv);
    for (i <- av.domain._1;
         j <- bv.domain._2) {
      rv(i,j) = av.getRow(i) dot bv.getCol(j);
    }
    rv;
  }
  
}

/** Right multiplication of a Tensor2 by a column. */
case class Tensor2MultColTensor1[I,J,VV<:Tensor1[J],MV<:Tensor2[I,J],Value<:Tensor1[I]]
(a : Tensor2Op[MV], b : ColTensor1Op[VV])
(implicit tpB: TensorProductBuilder[MV,VV,Value,Shape2,Shape1Col,Shape1Col])
extends ColTensor1Op[Value] {
  
  override def value = {
    val mv = a.value;
    val vv = b.value;
    val rv = tpB.create(mv,vv);
    val domain = mv.domain._1;
    for (i <- domain) {
      rv(i) = mv.getRow(i) dot vv;
    }
    rv;
  }
}
