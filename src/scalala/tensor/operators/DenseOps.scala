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

import scalala.collection.{MergeableSet,ProductSet};
import scalala.tensor.{Vector,Matrix,Tensor1,Tensor2};
import scalala.tensor.dense.{DenseVector,DenseMatrix};

import TensorShapes._;

import VectorTypes._;
import MatrixTypes._;

/** Type aliases supporting DenseVector operators. */
object DenseVectorTypes {
  type ColDenseVectorOp[V<:DenseVector] =
    ColVectorOp[V];
  
  type RowDenseVectorOp[V<:DenseVector] =
    RowVectorOp[V];
}

import DenseVectorTypes._;
import Tensor1Types._;
import Tensor2Types._;

/** Type aliases supporting DenseMatrix operators. */
object DenseMatrixTypes {
  type DenseMatrixOp[M<:DenseMatrix] =
    MatrixOp[M];

  type DenseMatrixTranspose[M<:DenseMatrix] =
    MatrixTranspose[M,Matrix];

}

import DenseMatrixTypes._;

/** Implicits supporting DenseVector operations. */
trait DenseVectorOps {
  implicit val denseVectorColArith = new Tensor1Arith[Int,DenseVector,Tensor1[Int],Shape1Col];
  implicit val denseVectorRowArith = new Tensor1Arith[Int,DenseVector,Tensor1[Int],Shape1Row];


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
}

/** Singleton instance of DenseVectorOps trait. */
object DenseVectorOps extends DenseVectorOps;

/** Implicits supporting DenseMatrix operations. */
trait DenseMatrixOps {
  implicit val denseMatrixArith = new TensorArith[(Int,Int),DenseMatrix,Tensor2[Int,Int],Shape2];

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

  implicit val denseMatrixPBuilder: TensorProductBuilder[DenseMatrix,Matrix,DenseMatrix,Shape2,Shape2,Shape2] = {
    new TensorProductBuilder[DenseMatrix,Matrix,DenseMatrix,Shape2,Shape2,Shape2] {
      def create(t: DenseMatrix, t2: Matrix) = new DenseMatrix(t.rows,t2.cols);
      def makeProduct(t: Tensor2Op[DenseMatrix], t2: Tensor2Op[Matrix]) = {
        Tensor2MultTensor2[Int,Int,Int,DenseMatrix,DenseMatrix,Matrix](t,t2);
      }
    }
  }

  implicit val denseMatrixVPBuilder : TensorProductBuilder[DenseMatrix,Vector,DenseVector,Shape2,Shape1Col,Shape1Col] =
    new TensorProductBuilder[DenseMatrix,Vector,DenseVector,Shape2,Shape1Col,Shape1Col] {
      def create(t: DenseMatrix, t2: Vector) = new DenseVector(t.rows);
      def makeProduct(t: Tensor2Op[DenseMatrix], t2: ColTensor1Op[Vector]) = {
        Tensor2MultColTensor1[Int,Int,Vector,DenseMatrix,DenseVector](t,t2);
      }
  }

}

/** Singleton instance of DenseMatrixOps trait. */
object DenseMatrixOps extends DenseMatrixOps;

/** Matrix solve vector, like matlab's "\", uses DenseMatrixSolveDenseMatrix. */
case class DenseMatrixSolveDenseVector[M<:DenseMatrix,V<:DenseVector]
(m : DenseMatrixOp[M], v : ColDenseVectorOp[V])
extends ColDenseVectorOp[DenseVector] {
  import OperatorImplicits._;
  
  /** Solves via repurposing the DenseMatrixSolveDenseMatrix code. */
  override lazy val value = {
    val _b = v.working;
    val _B = new DenseMatrix(_b.size, 1, _b.data);
    val _X = DenseMatrixSolveDenseMatrix[M,DenseMatrix](m, _B).value;
    
    new DenseVector(_X.data);
  }
  
}

class MatrixSingularException extends RuntimeException;

/**
 * DenseMatrix solver based on code from MTJ 0.9.9.
 */
case class DenseMatrixSolveDenseMatrix[M1<:DenseMatrix,M2<:DenseMatrix]
(a : DenseMatrixOp[M1], b : DenseMatrixOp[M2])
extends TensorReferenceOp[M1,Shape2](a) {
  import scalala.tensor.dense.Numerics;
  
  override lazy val value = {
    

    val aWorking = a.working;
    val _B = b.value; 
    
    val domain = ProductSet(aWorking.domain._2,
                            _B.domain.asInstanceOf[ProductSet[Int,Int]]._2);

    val rows = domain._1.size;
    val cols = domain._2.size;

    // from MTJ 0.9.9
    if (aWorking.domain._1 == aWorking.domain._2) { // square?
      // LUSolve
      val _A : DenseMatrix = aWorking; // will be overwritten
      val _B : DenseMatrix = b.value;   // won't be overwritten
      val rv = _A.matrixLike(rows,cols);

      if (_A.rows != _B.rows)
        throw new IllegalArgumentException("Matrix arguments must have same number of rows");
      if (rv.rows != _A.cols)
        throw new IllegalArgumentException("This matrix must have same number of rows as first argument has cols");
      if (rv.cols != _B.cols)
        throw new IllegalArgumentException("This matrix must have same number of cols as second argument has rows");
          
      rv := _B;
      val piv = new Array[Int](_A.rows);
      val info = Numerics.lapack.gesv(_A.rows, _B.cols, _A.data, piv, rv.data);
      if (info > 0)
        throw new MatrixSingularException();
      else if (info < 0)
        throw new IllegalArgumentException();
      rv.asInstanceOf[M1];
    } else {
      // QRSolve
      val (trans,_A) = a match {
        case Tensor2Transpose(aT) => (true,aT.working.asInstanceOf[M1]);
        case _ => (false, aWorking);
      }
      val rv = _A.matrixLike(rows,cols);
          
      // allocate temporary solution matrix
      val nrhs = _B.cols;
      val _Xtmp = new DenseMatrix(Math.max(rows,cols),nrhs);
      val M = if (!trans) _A.rows else _A.cols;
      for (j <- 0 until nrhs; i <- 0 until M) { _Xtmp(i,j) = _B(i,j); }
          
      // query optimal workspace
      val queryWork = new Array[Double](1);
      val queryInfo = Numerics.lapack.gels(trans, _A.rows, _A.cols, nrhs, _A.data, _Xtmp.data, queryWork, -1);
          
      // allocate workspace
      val work = {
        val lwork = {
          if (queryInfo != 0)
            Math.max(1, Math.min(_A.rows, _A.cols) + Math.max(Math.min(_A.rows, _A.cols), nrhs));
          else
            Math.max(queryWork(0).asInstanceOf[Int], 1);
        }
        new Array[Double](lwork);
      }
          
      // compute factorization
      val info = Numerics.lapack.gels(trans, _A.rows, _A.cols, nrhs, _A.data, _Xtmp.data, work, work.length);
          
      if (info < 0)
        throw new IllegalArgumentException;
          
      // extract solution
      val N = if (!trans) _A.cols else _A.rows;
      for (j <- 0 until nrhs; i <- 0 until N) rv(i,j) = _Xtmp(i,j);
      rv.asInstanceOf[M1];
    }
    
  }
}

//c <- alpha * a * b + beta * c
//case class GEMM(alpha : Double, a : MatrixOp[DenseMatrix], b : MatrixOp[DenseMatrix],
//                beta : Double, c : MatrixOp[DenseMatrix])
//extends TensorReferenceOp(a) {
//  if ((a.rows != c.rows) ||
//      (a.cols != b.rows) ||
//      (b.cols != c.cols))
//    throw new IllegalArgumentException("Wrong dimensions for matrix multiplication");
//    
//  override def domain = c.domain;
//  override def value = {
//    val (transA,_A) = a match {
//      case aa : (Tensor2Transpose[i1,i2,m1,m2,v1,v2] with MatrixOp) => (true, aa.op.value.asInstanceOf[DenseMatrix]);
//      case _ =>                   (false, a.value.asInstanceOf[DenseMatrix]);
//    }
//        
//    val (transB,_B) = b match {
//      case bb : (Tensor2Transpose[i1,i2,m1,m2,v1,v2] with MatrixOp) => (true, bb.op.value.asInstanceOf[DenseMatrix]);
//      case _ =>                   (false, b.value.asInstanceOf[DenseMatrix]);
//    }
//      
//    val _C = c.working;
//      
//    Numerics.blas.gemm(transA, transB, _C.rows, _C.cols, _A.cols,
//                       alpha, _A.data, _A.rows, _B.data, _B.rows,
//                       beta, _C.asInstanceOf[DenseMatrix].data, _C.rows);
//      
//    _C;
//  }
//}

