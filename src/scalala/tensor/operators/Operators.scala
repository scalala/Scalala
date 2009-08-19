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

  implicit def tensorBuilder[I] = new TensorBuilder[Tensor[I]] {
    def like(t: Tensor[I]) = t.like;
  }

  implicit def tensor1Builder[I] = new TensorBuilder[Tensor1[I]] {
    def like(t: Tensor1[I]) = t.like;
  }

  import scalala.tensor.dense._;
  import scalala.tensor.sparse._;

  import Tensor1Types._;
  import Tensor2Types._;

  implicit val mtranspose = new MatrixTranspose[Matrix,Matrix] {
    def makeTranspose(op: Tensor2Op[Matrix]) = Tensor2Transpose[Int,Int,Matrix,Matrix](op);
  }

  implicit val dmtranspose = new MatrixTranspose[DenseMatrix,DenseMatrix] {
    def makeTranspose(op: Tensor2Op[DenseMatrix]) = Tensor2Transpose[Int,Int,DenseMatrix,DenseMatrix](op);
  }

  implicit val denseMatrixSolveDenseVector: TensorSolver[DenseMatrix,DenseVector,DenseVector,Shape1Col,Shape1Col] =
  new TensorSolver[DenseMatrix,DenseVector,DenseVector,Shape1Col,Shape1Col] {
    def solve(m: TensorOp[DenseMatrix,Shape2], v: TensorOp[DenseVector,Shape1Col]): TensorOp[DenseVector,Shape1Col] =  {
      DenseMatrixSolveDenseVector(m,v);
    }
  }

  implicit val denseMatrixSolveDenseMatrix = new TensorSolver[DenseMatrix,DenseMatrix,DenseMatrix,Shape1Col,Shape2] {
    def solve(m: TensorOp[DenseMatrix,Shape2], m2: TensorOp[DenseMatrix,Shape2]): TensorOp[DenseMatrix,Shape2] =  {
      DenseMatrixSolveDenseMatrix(m,m2);
    }
  }

  implicit val vectorColArith = new Tensor1Arith[Int,Vector,Tensor1[Int],Shape1Col];
  implicit val vectorRowArith = new Tensor1Arith[Int,Vector,Tensor1[Int],Shape1Row];
  implicit val denseVectorColArith = new Tensor1Arith[Int,DenseVector,Tensor1[Int],Shape1Col];
  implicit val denseVectorRowArith = new Tensor1Arith[Int,DenseVector,Tensor1[Int],Shape1Row];
  implicit val matrixArith = new TensorArith[(Int,Int),Matrix,Tensor2[Int,Int],Shape2];
  implicit val denseMatrixArith = new TensorArith[(Int,Int),DenseMatrix,Tensor2[Int,Int],Shape2];
  implicit def tensor1ColArith[I] = new Tensor1Arith[I,Tensor1[I],Tensor1[I],Shape1Col];
  implicit def tensor1RowArith[I] = new TensorArith[I,Tensor1[I],Tensor1[I],Shape1Row];
  implicit def tensor2Arith[I,J] = new TensorArith[(I,J),Tensor2[I,J],Tensor2[I,J],Shape2];
  implicit def tensorArith[I] = new TensorArith[I,Tensor[I],Tensor[I],AnyShape];

  implicit val vectorBuilder = new TensorBuilder[Vector] {
    def like(t: Vector): Vector =  t.like;
  }

  implicit val vectorPBuilder : TensorProductBuilder[Vector,Vector,Matrix,Shape1Col,Shape1Row,Shape2] = {
    new TensorProductBuilder[Vector,Vector,Matrix,Shape1Col,Shape1Row,Shape2] {
      def create(t: Vector,t2: Vector) = t.matrixLike(t.size,t2.size);
      def makeProduct(t: ColTensor1Op[Vector], t2: RowTensor1Op[Vector]) = {
        Tensor1OuterMultTensor1[Int,Int,Vector,Vector,Matrix](t,t2);
      }
    }
  }

  implicit val vectorMPBuilder : TensorProductBuilder[Vector,Matrix,Vector,Shape1Row,Shape2,Shape1Row] = {
    new TensorProductBuilder[Vector,Matrix,Vector,Shape1Row,Shape2,Shape1Row] {
      def create(t: Vector,t2: Matrix) = t.vectorLike(t2.cols);
      def makeProduct(t: RowTensor1Op[Vector], t2: Tensor2Op[Matrix]) = {
        RowTensor1MultTensor2[Int,Int,Vector,Vector,Matrix](t,t2);
      }
    }
  }

  implicit val sparseHashVBuilder = new TensorBuilder[SparseHashVector] {
    def like(t: SparseHashVector): SparseHashVector = t.like;
  }

  implicit val sparseVBuilder = new TensorBuilder[SparseVector] {
    def like(t: SparseVector): SparseVector = t.like;
  }

  implicit val denseVectorBuilder = new TensorBuilder[DenseVector] {
    def like(t: DenseVector) = t.like;
  }

  implicit val denseVectorPBuilder: TensorProductBuilder[DenseVector,Vector,DenseMatrix,Shape1Col,Shape1Row,Shape2] =
  new TensorProductBuilder[DenseVector,Vector,DenseMatrix,Shape1Col,Shape1Row,Shape2] {
    def create(t: DenseVector, t2: Vector) = new DenseMatrix(t.size,t2.size);
    def makeProduct(t: ColTensor1Op[DenseVector], t2: RowTensor1Op[Vector]) = {
      Tensor1OuterMultTensor1[Int,Int,DenseVector,Vector,DenseMatrix](t,t2);
    }
  }

  class DenseVectorMPBuilder extends TensorProductBuilder[DenseVector,Matrix,DenseVector,Shape1Row,Shape2,Shape1Row] {
    def create(t: DenseVector,t2: Matrix) = t.vectorLike(t2.cols);
    def makeProduct(t: RowTensor1Op[DenseVector], t2: Tensor2Op[Matrix]) = {
      RowTensor1MultTensor2[Int,Int,DenseVector,DenseVector,Matrix](t,t2);
    }
  }

  implicit val denseVectorMPBuilder = new DenseVectorMPBuilder;

  implicit val matrixBuilder = new TensorBuilder[Matrix] {
    def like(t: Matrix) = t.like;
  }

  implicit val matrixVPBuilder : TensorProductBuilder[Matrix,Vector,Vector,Shape2,Shape1Col,Shape1Col] = {
    new TensorProductBuilder[Matrix,Vector,Vector,Shape2,Shape1Col,Shape1Col] {
      def create(t: Matrix, t2: Vector):Vector = t.vectorLike(t.rows);
      def makeProduct(t: Tensor2Op[Matrix], t2: ColTensor1Op[Vector]) = {
        Tensor2MultColTensor1[Int,Int,Vector,Matrix,Vector](t,t2);
      }
    }
  }

  implicit val matrixPBuilder : TensorProductBuilder[Matrix,Matrix,Matrix,Shape2,Shape2,Shape2] = {
    new TensorProductBuilder[Matrix,Matrix,Matrix,Shape2,Shape2,Shape2] {
      def create(t: Matrix, t2: Matrix):Matrix = t.matrixLike(t.rows,t2.cols);
      def makeProduct(t: Tensor2Op[Matrix], t2: Tensor2Op[Matrix]) = {
        Tensor2MultTensor2[Int,Int,Int,Matrix,Matrix,Matrix](t,t2);
      }
    }
  }

  implicit val denseMatrixPBuilder: TensorProductBuilder[DenseMatrix,Matrix,DenseMatrix,Shape2,Shape2,Shape2] = {
    new TensorProductBuilder[DenseMatrix,Matrix,DenseMatrix,Shape2,Shape2,Shape2] {
      def create(t: DenseMatrix, t2: Matrix) = new DenseMatrix(t.rows,t2.cols);
      def makeProduct(t: Tensor2Op[DenseMatrix], t2: Tensor2Op[Matrix]) = {
        Tensor2MultTensor2[Int,Int,Int,DenseMatrix,DenseMatrix,Matrix](t,t2);
      }
    }
  }

  implicit val denseMatrixBuilder = new TensorBuilder[DenseMatrix] {
    def like(t: DenseMatrix) = new DenseMatrix(t.rows,t.cols);
  }

  implicit val denseMatrixVPBuilder : TensorProductBuilder[DenseMatrix,Vector,DenseVector,Shape2,Shape1Col,Shape1Col] =
    new TensorProductBuilder[DenseMatrix,Vector,DenseVector,Shape2,Shape1Col,Shape1Col] {
      def create(t: DenseMatrix, t2: Vector) = new DenseVector(t.rows);
      def makeProduct(t: Tensor2Op[DenseMatrix], t2: ColTensor1Op[Vector]) = {
        Tensor2MultColTensor1[Int,Int,Vector,DenseMatrix,DenseVector](t,t2);
      }
  }

  implicit def iTensorOpToRichTensor[V<:Tensor[_]](t: TensorOp[V,AnyShape]):RichTensorOp[V,AnyShape] = {
    new RichTensorOp[V,AnyShape](t);
  }

  implicit def iColTensor1OpToRichColTensor1Op[V<:Tensor1[_]]
  (op : TensorOp[V,Tensor1Op.Col]) =
    new RichColTensor1Op[V](op);

  implicit def iSelfTensor1OpToRichTensor[V<:Tensor1[_] with TensorSelfOp[_,V,Shape1Col]] 
      (op : V) = new RichColTensor1Op[V](op);

  implicit def iRowTensor1OpToRichRowTensor1Op[V<:Tensor1[_]]
  (op : RowTensor1Op[V]) =
    new RichRowTensor1Op[V](op);

  implicit def iTensorX2OpToRichTensor2Op[V<:Tensor2[_,_] with TensorSelfOp[_,V,Shape2]]
  (op : V):RichTensor2Op[V]  = 
    new RichTensor2Op[V](op);
  
  
  implicit def iTensor2OpToRichTensor2Op[V<:Tensor2[_,_]]
  (op : Tensor2Op[V]) =
    new RichTensor2Op[V](op);
  
  import VectorTypes._;
  import scalala.tensor._;
  import scalala.tensor.dense._;
  
  import MatrixTypes._;
  
  import DenseVectorTypes._;
  
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
