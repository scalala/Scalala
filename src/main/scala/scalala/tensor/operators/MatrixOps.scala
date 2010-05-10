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

/** Type aliases supporting Matrix operations. */
object MatrixTypes {
  type MatrixOp[M<:Matrix] =
    TensorOp[M,Shape2];
}

import MatrixTypes._;
import VectorTypes._;
import Tensor1Types._;
import Tensor2Types._;

/** Implicits supporting Matrix operations. */
trait MatrixOps {

  implicit val matrixArith = new TensorArith[(Int,Int),Matrix,Tensor2[Int,Int],Shape2];

  implicit val mtranspose = new MatrixTranspose[Matrix,Matrix] {
    def makeTranspose(op: Tensor2Op[Matrix]) ={
      MatrixTransOp(op);
    }
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


}

/** Type-safe transposes of a Matrix. */
case class MatrixTransOp
(op : Tensor2Op[Matrix])
extends MatrixOp[Matrix] {
  override def value = op.value.transpose;
  override def working = op.working.transpose;
}

/** Singleton instance of MatrixOps trait. */
object MatrixOps extends MatrixOps;
