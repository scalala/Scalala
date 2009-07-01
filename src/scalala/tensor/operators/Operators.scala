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

/**
 * Standard operator library includes TensorOps, Tensor1Ops, VectorOps,
 * DenseVectorOps, Tensor2Ops, MatrixOps, and DenseMatrixOps.
 */
trait OperatorImplicits extends TensorOps 
with Tensor1Ops with VectorOps with DenseVectorOps with SparseBinaryVectorOps
with Tensor2Ops with MatrixOps with DenseMatrixOps {
  
  import TensorShapes._;
  
  implicit def iTensorOpToTensor[I,Base<:Tensor[I],V<:Base,S<:PublicShape]
   (x : TensorOp[I,Base,V,S]) : V = x.value;
  
 
//  implicit def iTensorToTensorOp[I,V<:Tensor[I]](tensor : V) =
//    TensorIdentity[I,Tensor[I],V,AnyShape](tensor);
  implicit def iTensorToTensorOp[I](tensor : Tensor[I]) =
    TensorIdentity[I,Tensor[I],Tensor[I],AnyShape](tensor);

//  implicit def iTensorToRichTensorOp[I,V<:Tensor[I]](tensor : V) =
//    new RichTensorOp[I,Tensor[I],V,AnyShape](tensor);
  implicit def iTensorToRichTensorOp[I](tensor : Tensor[I]) =
    new RichTensorOp[I,Tensor[I],Tensor[I],AnyShape](tensor);
  
//  implicit def iTensorOpToRichTensorOp[I,V<:Tensor[I]](op : TensorOp[I,Tensor[I],V,AnyShape]) =
//    new RichTensorOp[I,Tensor[I],V,AnyShape](op);
  implicit def iTensorOpToRichTensorOp[I](op : TensorOp[I,Tensor[I],Tensor[I],AnyShape]) =
    new RichTensorOp[I,Tensor[I],Tensor[I],AnyShape](op);

  import Tensor1Types._;
  
//  implicit def iTensor1ToColTensor1Op[I,V<:Tensor1[I]](tensor : V) =
//    TensorIdentity[I,Tensor1[I],V,Tensor1Op.Col](tensor);
  implicit def iTensor1ToColTensor1Op[I](tensor : Tensor1[I]) =
    TensorIdentity[I,Tensor1[I],Tensor1[I],Tensor1Op.Col](tensor);

//  implicit def iTensor1ToRichColTensor1Op[I,V<:Tensor1[I]](tensor : V) =
//    new RichColTensor1Op[I,Tensor1[I],V,Tensor2[I,I]](tensor);
  implicit def iTensor1ToRichColTensor1Op[I](tensor : Tensor1[I]) =
    new RichColTensor1Op[I,Tensor1[I],Tensor1[I],Tensor2[I,I]](tensor);
  
//  implicit def iColTensor1OpToRichColTensor1Op[I,V<:Tensor1[I]]
//  (op : TensorOp[I,Tensor1[I],V,Tensor1Op.Col]) =
//    new RichColTensor1Op[I,Tensor1[I],V,Tensor2[I,I]](op);
  implicit def iColTensor1OpToRichColTensor1Op[I]
  (op : TensorOp[I,Tensor1[I],Tensor1[I],Tensor1Op.Col]) =
    new RichColTensor1Op[I,Tensor1[I],Tensor1[I],Tensor2[I,I]](op);

//  implicit def iRowTensor1OpToRichRowTensor1Op[I,V<:Tensor1[I]]
//  (op : RowTensor1Op[I,Tensor1[I],V]) =
//    new RichRowTensor1Op[I,Tensor1[I],V](op);
  implicit def iRowTensor1OpToRichRowTensor1Op[I]
  (op : RowTensor1Op[I,Tensor1[I],Tensor1[I]]) =
    new RichRowTensor1Op[I,Tensor1[I],Tensor1[I]](op);
  
  import Tensor2Types._;
  
//  implicit def iTensor2ToTensor2Op[I,J,V<:Tensor2[I,J]](tensor : V) =
//    TensorIdentity[(I,J),Tensor2[I,J],V,Shape2[I,J]](tensor);
  implicit def iTensor2ToTensor2Op[I,J](tensor : Tensor2[I,J]) =
    TensorIdentity[(I,J),Tensor2[I,J],Tensor2[I,J],Shape2[I,J]](tensor);
  
//  implicit def iTensor2ToRichTensor2Op[I,J,V<:Tensor2[I,J]](x : V) =
//    new RichTensor2Op[I,J,Tensor2,Tensor2,V,Tensor1](x);
  implicit def iTensor2ToRichTensor2Op[I,J](x : Tensor2[I,J]) =
    new RichTensor2Op[I,J,Tensor2,Tensor2,Tensor2[I,J],Tensor1](x);
  
//  implicit def iTensor2OpToRichTensor2Op[I,J,V<:Tensor2[I,J]]
//  (op : Tensor2Op[I,J,Tensor2[I,J],V]) =
//    new RichTensor2Op[I,J,Tensor2,Tensor2,V,Tensor1](op);
  implicit def iTensor2OpToRichTensor2Op[I,J]
  (op : Tensor2Op[I,J,Tensor2[I,J],Tensor2[I,J]]) =
    new RichTensor2Op[I,J,Tensor2,Tensor2,Tensor2[I,J],Tensor1](op);
  
  import VectorTypes._;
  import scalala.tensor._;
  import scalala.tensor.dense._;
  
  implicit def iColVectorOpTopRichColVectorOp[V<:Vector](op : ColVectorOp[V]) =
    new RichColVectorOp(op);

  implicit def iRowVectorOpToRichRowVectorOp[V<:Vector](op : RowVectorOp[V]) =
    new RichRowVectorOp(op);
  
  implicit def iVectorToColVectorOp[V <: Vector](vector : V) =
    new TensorIdentity[Int,Vector,V,Tensor1Op.Col](vector);
 
  implicit def iVectorToRichColVectorOp[V <: Vector](vector : V) =
    new RichColVectorOp[V](new TensorIdentity[Int,Vector,V,Tensor1Op.Col](vector));
  
  import MatrixTypes._;
  
  implicit def iMatrixToMatrixOp[M<:Matrix](m : M) =
    new TensorIdentity[(Int,Int),Matrix,M,Shape2[Int,Int]](m);
  
  implicit def iMatrixToRichMatrixOp[M<:Matrix](m : M) =
    new RichMatrixOp(m);
  
  implicit def iMatrixOpToRichMatrixOp[M<:Matrix,V<:Vector]
  (op : MatrixOp[M]) =
    new RichMatrixOp(op);
  
  import DenseVectorTypes._;
  
  implicit def iDenseVectorToColDenseVectorOp[V<:DenseVector](vector : V) =
    new TensorIdentity[Int,Vector,V,Tensor1Op.Col](vector);
  
  implicit def iDenseVectorToRichColDenseVectorOp[V<:DenseVector](vector : V) =
    new RichColDenseVectorOp[V](vector);
  
  implicit def iColDenseVectorOpTopRichColVectorOp[V<:DenseVector](op : ColDenseVectorOp[V]) =
    new RichColDenseVectorOp(op);

  implicit def iRowDenseVectorOpToRichRowDenseVectorOp[V<:DenseVector](op : RowDenseVectorOp[V]) =
    new RichRowDenseVectorOp(op);
  
  implicit def iArrayToColDenseVectorOp(array : Array[Double])
  : ColDenseVectorOp[DenseVector] =
    new DenseVector(array);
  
  implicit def iArrayToRichColVectorOp(array : Array[Double]) =
    new RichColDenseVectorOp(new DenseVector(array));
  
  import DenseMatrixTypes._;
  
  implicit def iDenseMatrixToDenseMatrixOp[M<:DenseMatrix](m : M) =
    iMatrixToMatrixOp(m);
  
  implicit def iDenseMatrixToRichDenseMatrixOp[M<:DenseMatrix](m : M) =
    new RichDenseMatrixOp(m);
  
  implicit def iDenseMatrixOpToRichDenseMatrixOp[M<:DenseMatrix](op : DenseMatrixOp[M]) =
    new RichDenseMatrixOp(op);
  
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
//  val a = iScalarToRichScalarTensorOp(2) + w;
  val b = w.t;

  val qq : DenseMatrix = scalala.Scalala.ones(3,3);
  val qq1 = qq + 1;
  val qq2 = qq + 1 - qq;
  val qq3 = qq1.t;
  val qq4 = qq2 \ dense;
  val qq5 = qq :* qq1;
  qq5 > qq3;

  val yy = x.t * qq;
  val yy1 = (x+1).t * qq;
  val yy2 = qq * qq;
  val yy3 = qq.t * qq;
  val yy4 = qq * x;
  
//  val sparse = new SparseBinaryVector(10);
//  sparse(2) = 1;
//  sparse(7) = 1;
//  sparse.t * sparse;
//  sparse.t * (sparse + 1);
}
