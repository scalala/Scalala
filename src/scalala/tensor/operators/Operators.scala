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
import scalala.tensor._;
import scalala.tensor.dense._;
import scalala.tensor.sparse._;

/**
 * Standard operator library includes TensorOps, Tensor1Ops, VectorOps,
 * DenseVectorOps, Tensor2Ops, MatrixOps, and DenseMatrixOps.
 */
trait OperatorImplicits extends TensorOps 
    with Tensor1Ops with VectorOps with DenseVectorOps with SparseBinaryVectorOps
    with Tensor2Ops with MatrixOps with DenseMatrixOps {
  import TensorShapes._;
  
  implicit def iTensorOpToTensor[V<:Tensor[_],S<:PublicShape](x : TensorOp[V,S]) : V = x.value;

  import scalala.tensor.dense._;
  import scalala.tensor.sparse._;

  import Tensor1Types._;
  import Tensor2Types._;

  // We can't offload implicit conversions to the correct traits because of
  // weirdness in how scala handles overloads in inheritance for implicits. Meh.
 
  implicit def iTensorOpToRichTensor[V<:Tensor[_]](t: TensorOp[V,AnyShape]):RichTensorOp[V,AnyShape] = {
    new RichTensorOp[V,AnyShape](t);
  }

  implicit def iSelfTensor1OpToRichTensor[V<:Tensor1[_] with TensorSelfOp[_,V,Shape1Col]] 
      (op : V) = new RichColTensor1Op[V](op);

  implicit def iRowTensor1OpToRichRowTensor1Op[V<:Tensor1[_]]
  (op : RowTensor1Op[V]) =
    new RichRowTensor1Op[V](op);

  implicit def iColTensor1OpToRichColTensor1Op[V<:Tensor1[_]]
  (op : TensorOp[V,Tensor1Op.Col]) =
    new RichColTensor1Op[V](op);


  implicit def iTensorX2OpToRichTensor2Op[V<:Tensor2[_,_] with TensorSelfOp[_,V,Shape2]]
  (op : V):RichTensor2Op[V]  = 
    new RichTensor2Op[V](op);
  
  
  implicit def iTensor2OpToRichTensor2Op[V<:Tensor2[_,_]]
  (op : Tensor2Op[V]) =
    new RichTensor2Op[V](op);
  
  import VectorTypes._;
  import scalala.tensor._;
  import scalala.tensor.dense._;
  
  implicit def iArrayToRichColVectorOp(array : Array[Double]) =
    new RichColTensor1Op(new DenseVector(array));
  
  import DenseMatrixTypes._;
  
 // def iScalarToRichScalarTensorOp(scalar: Double) = new RichScalarTensorOp(scalar);
}

/**
 * Singleton object for OperatorImplicits trait.
 */
object OperatorImplicits extends OperatorImplicits;

/** Some scratch space for making sure the right things compile. */
private object Scratch {
  import scalala.tensor._;
  import scalala.tensor.dense._;
  import scalala.tensor.sparse._;
  
  import OperatorImplicits._;
  
  val dense : DenseVector = scalala.Scalala.Vector(1,2,3).asInstanceOf[DenseVector]
  val vector = scalala.Scalala.Vector(1,2,3).asInstanceOf[scalala.tensor.Vector];

  val mixed = vector + dense;

  val x = vector + 3;
  val y = x + 2;
//  val lkdfjkf = iScalarToRichScalarTensorOp(2) * x;
  val z = y.t;
  val zz = z.t;
  val w = (z + 1).t;
  val kdjf = x.t * x;
  val kqke = x * x.t;

  //z + z.t
  val c = -x;
  c.t;
  val ww = (c.t > 2) + 1;

  val bb = x + x;
  //val a = 2. + w;
  val b = w.t;

  val qq : DenseMatrix = scalala.Scalala.ones(3,3);
  val qq1 = qq + 1;
  val qq2 = qq + 1 - qq;
  val qq3 = qq1.t;
  val qq4 = qq2 \ dense;
  val qq5 = qq :* qq1;
  qq5 > qq3;

  val yy = x.t.*(qq);
  val yy1 = (x+1).t * qq;
  val yy2x = qq *(qq);
  val yy2 = qq * qq;
  val yy3 = qq.t * qq;
  val yy4 : DenseVector = qq * x  value;

  val d1 = yy4.like;
  d1 :+= yy4;
  d1 :*= yy4;
  d1 :^= yy4;
  d1 :/= yy4;
  d1 :-= yy4;
  
//  val sparse = new SparseBinaryVector(10);
//  sparse(2) = 1;
//  sparse(7) = 1;
//  sparse.t * sparse;
//  sparse.t * (sparse + 1);
}
